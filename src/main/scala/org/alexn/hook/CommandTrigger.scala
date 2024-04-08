package org.alexn.hook

import cats.data.EitherT
import cats.effect.kernel.{Async, Sync}
import cats.effect.std.Semaphore
import cats.effect.syntax.all.*
import cats.syntax.all.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*

final class CommandTrigger[F[_]](
    projects: Map[String, ProjectConfig],
    locks: AtomicReference[Map[String, Semaphore[F]]]
)(using
    Async[F],
    OperatingSystem[F]
):
    def lockFor(key: String): F[Semaphore[F]] =
        locks.modifyDelayed: locks =>
            locks.get(key) match
            case Some(mutex) => Right(mutex)
            case None =>
                Left(Semaphore[F](1).map: mutex =>
                    (locks.updated(key, mutex), mutex))

    def triggerCommand(key: String): EitherT[F, RequestError, Unit] =
        for
            project <- EitherT.fromOption(
                projects.get(key),
                RequestError.NotFound(s"Project not found: $key")
            )
            result <- EitherT:
                val timeoutDuration = project.timeout.getOrElse(30.seconds)
                val task =
                    for
                        mutex <- lockFor(key)
                        result <-
                            mutex.permit.use: _ =>
                                OperatingSystem[F].executeRawShellCommand(
                                    project.command,
                                    dir = Some(project.directory)
                                )
                            .timeout(timeoutDuration)
                    yield
                        if result.exitCode == 0 then
                            Right(())
                        else
                            Left(RequestError.InternalServerError(
                                s"Command execution failed for `$key``",
                                cause = null,
                                meta = Map(
                                    "exit-code" -> result.exitCode.toString,
                                    "stdout" -> result.stdout,
                                    "stderr" -> result.stderr
                                )
                            ))
                task.recover:
                    case _: TimeoutException =>
                        Left(RequestError.TimedOut(
                            s"Command execution timed-out after $timeoutDuration"
                        ))
        yield result
end CommandTrigger

object CommandTrigger:
    def apply[F[_]](projects: Map[String, ProjectConfig])(using
        Async[F],
        OperatingSystem[F]
    ): F[CommandTrigger[F]] =
        Sync[F].delay:
            val ref = new AtomicReference[Map[String, Semaphore[F]]](Map.empty)
            new CommandTrigger[F](projects, ref)
