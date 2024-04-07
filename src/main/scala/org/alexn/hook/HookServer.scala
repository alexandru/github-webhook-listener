package org.alexn.hook

import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s.*
import fs2.io.net.Network
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.Logger

object HookServer:

    def run: IO[Nothing] =
        val res =
            for
                client <- EmberClientBuilder.default[IO].build
                helloWorldAlg = HelloWorld.impl
                jokeAlg = Jokes.impl(client)

                // Combine Service Routes into an HttpApp.
                // Can also be done via a Router if you
                // want to extract segments not checked in the underlying routes.
                httpApp = (
                    HookRoutes.helloWorldRoutes(helloWorldAlg) <+>
                        HookRoutes.jokeRoutes(jokeAlg)
                ).orNotFound

                // With Middlewares in place
                finalHttpApp = Logger.httpApp(true, true)(httpApp)

                _ <-
                    EmberServerBuilder.default[IO]
                        .withHost(ipv4"0.0.0.0")
                        .withPort(port"8080")
                        .withHttpApp(finalHttpApp)
                        .build
            yield ()
        res.useForever
    end run

end HookServer
