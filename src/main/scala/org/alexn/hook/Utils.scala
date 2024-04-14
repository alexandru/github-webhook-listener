package org.alexn.hook

import cats.effect.kernel.Sync
import cats.syntax.all.*
import io.circe.Codec

import java.util.concurrent.atomic.AtomicReference
import scala.deriving.Mirror

extension (self: Codec.type)
    inline def derived[A](using inline A: Mirror.Of[A]): Codec.AsObject[A] =
        io.circe.generic.semiauto.deriveCodec

extension [A](self: A | Null)
    def ?:(default: A): A =
        self match
        case null => default
        case _ => self.asInstanceOf[A]

extension [S](self: AtomicReference[S])
    inline def modify[R](inline f: S => (S, R)): R =
        var ret: R | Null = null
        var hasRet = false
        while !hasRet do
            val current = self.get
            val (next, r) = f(current)
            hasRet = current == next || self.compareAndSet(current, next)
            if hasRet then ret = r
        ret.asInstanceOf[R]

    def modifyDelayed[F[_], R](f: S => Either[F[(S, R)], R])(using Sync[F]): F[R] =
        Sync[F].defer:
            val current = self.get
            f(current) match
            case Right(r) => r.pure
            case Left(fsr) =>
                fsr.flatMap: (next, r) =>
                    if current == next || self.compareAndSet(current, next) then
                        r.pure
                    else // retry
                        modifyDelayed(f)
