package org.alexn.hook

import io.circe.Codec
import scala.deriving.Mirror

extension (self: Codec.type)
    inline def derived[A](using inline A: Mirror.Of[A]) =
        io.circe.generic.semiauto.deriveCodec

extension [A](self: A | Null)
    def ?:(default: A): A =
        self match
        case null => default
        case _ => self.asInstanceOf[A]
