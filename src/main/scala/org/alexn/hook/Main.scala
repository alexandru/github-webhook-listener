package org.alexn.hook

import cats.effect.kernel.Async
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.Host
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import fs2.io.net.Network
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.{Router, Server}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.tapir.server.http4s.{Http4sServerInterpreter, Http4sServerOptions}

def startServer[F[_]](config: HttpConfig)(using Async[F], Logger[F], Network[F]): Resource[F, Server] =
    val serverOptions: Http4sServerOptions[F] =
        Http4sServerOptions.default
    val routes =
        Http4sServerInterpreter[F](serverOptions).toRoutes(Endpoints[F].all)
    val host =
        config.host.getOrElse(Host.fromString("localhost").get)
    EmberServerBuilder
        .default[F]
        .withHost(host)
        .withPort(config.port)
        .withHttpApp(Router(config.path.getOrElse("/") -> routes).orNotFound)
        .build
        .evalTap: _ =>
            for
                _ <- Logger[F].info(s"Server started at http://$host:${config.port}")
                _ <- Logger[F].info(s"Navigate to http://$host:${config.port}/docs for the API documentation")
            yield ()

object Main extends CommandIOApp(
        name = "github-webhook-listener",
        header = "Light server for reacting to GitHub's Webhooks",
        version = "3.0.0"
    ):

    given Logger[IO] = Slf4jLogger.getLogger[IO]

    val config = Opts
        .argument[String]("Path to the configuration file")
        .mapValidated: path =>
            val file = new java.io.File(path)
            Either.cond(
                file.exists,
                file,
                s"File $path does not exist"
            ).toValidatedNel
        .map: file =>
            AppConfig.loadYaml[SyncIO](file)
                .rethrow
                .unsafeRunSync()

    override def main: Opts[IO[ExitCode]] =
        config.map: config =>
            startServer[IO](config.http)
                .use: _ =>
                    IO.println(s"Server started at ${config.http.host.getOrElse(
                            Host.fromString("localhost").get
                        )}:${config.http.port}")
                    IO.never
                .as(ExitCode.Success)
