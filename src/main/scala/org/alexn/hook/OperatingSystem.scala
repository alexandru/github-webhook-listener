package org.alexn.hook

import cats.effect.Outcome
import cats.effect.kernel.Async
import cats.effect.std.Env
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.apache.commons.text.StringEscapeUtils
import org.typelevel.log4cats
import org.typelevel.log4cats.Logger

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

final case class CommandResult(
    exitCode: Int,
    stdout: String,
    stderr: String
)

class OperatingSystem[F[_]](using Async[F], Env[F], Logger[F]):
    def executeCommand(
        executable: Path,
        args: List[String],
        dir: Option[File]
    ): F[CommandResult] =
        Async[F].blocking:
            val commandArgs = executable.toAbsolutePath.toString +: args
            Runtime.getRuntime.exec(
                commandArgs.toArray,
                Array.empty[String],
                dir.orNull
            )
        .bracketCase: proc =>
            // These aren't "interruptible", what actually interrupts them
            // is proc.destroy(); and due to how they are used, it's better
            // to not declare them as interruptible, as to not mislead:
            val collectStdout = Async[F].blocking:
                new String(proc.getInputStream.readAllBytes(), UTF_8)
            val collectStderr = Async[F].blocking:
                new String(proc.getErrorStream.readAllBytes(), UTF_8)
            for
                // Starts jobs asynchronously
                stdoutFiber <- collectStdout.start
                stderrFiber <- collectStderr.start
                // Waits for process to complete (this is actually interruptible)
                code <- Async[F].interruptible(proc.waitFor())
                // Reads output
                stdout <- stdoutFiber.joinWithNever
                stderr <- stderrFiber.joinWithNever
            yield CommandResult(code, stdout, stderr)
        .apply: (proc, outcome) =>
            for
                _ <- outcome match
                case Outcome.Canceled() => Logger[F].warn("Command was cancelled")
                case Outcome.Errored(e) => Logger[F].error(e)("Command failed")
                case Outcome.Succeeded(_) => Logger[F].debug("Command succeeded")
                _ <- Async[F].blocking(proc.destroy())
            yield ()

    def executeRawShellCommand(
        command: String,
        dir: Option[File]
    ): F[CommandResult] =
        executeCommand(
            executable = Path.of("/bin/sh"),
            args = List("-c", command),
            dir = dir
        )

    def executeEscapedShellCommand(
        command: String,
        args: List[String],
        dir: Option[File]
    ): F[CommandResult] =
        executeRawShellCommand(
            (command +: args).map(StringEscapeUtils.escapeXSI).mkString(" "),
            dir
        )

    def userHome: F[Option[File]] =
        for
            sysProp <- Async[F].delay(Option(System.getProperty("user.home")))
            envVar <- Env[F].get("HOME")
        yield sysProp
            .filter(_.nonEmpty)
            .orElse(envVar)
            .filter(_.nonEmpty)
            .map(File(_))

end OperatingSystem

object OperatingSystem:
    def apply[F[_]](using Async[F], Env[F], Logger[F]): OperatingSystem[F] =
        new OperatingSystem[F]
