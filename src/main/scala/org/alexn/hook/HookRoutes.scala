package org.alexn.hook

import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

object HookRoutes:
    object dsl extends Http4sDsl[IO]
    import dsl.*

    def jokeRoutes(j: Jokes[IO]): HttpRoutes[IO] =
        HttpRoutes.of[IO]:
            case GET -> Root / "joke" =>
                for
                    joke <- j.get
                    resp <- Ok(joke)
                yield resp

    def helloWorldRoutes(h: HelloWorld): HttpRoutes[IO] =
        HttpRoutes.of[IO]:
            case GET -> Root / "hello" / name =>
                for
                    greeting <- h.hello(HelloWorld.Name(name))
                    resp <- Ok(greeting)
                yield resp

end HookRoutes
