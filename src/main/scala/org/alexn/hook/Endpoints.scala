package org.alexn.hook

import cats.effect.IO
import io.circe.generic.auto.*
import org.alexn.hook.Library.*
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.json.circe.*
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.swagger.bundle.SwaggerInterpreter

object Endpoints:
    case class User(name: String) extends AnyVal
    val helloEndpoint: PublicEndpoint[User, Unit, String, Any] = endpoint.get
        .in("hello")
        .in(query[User]("name"))
        .out(stringBody)
    val helloServerEndpoint: ServerEndpoint[Any, IO] =
        helloEndpoint.serverLogicSuccess(user => IO.pure(s"Hello ${user.name}"))

    val booksListing: PublicEndpoint[Unit, Unit, List[Book], Any] = endpoint.get
        .in("books" / "list" / "all")
        .out(jsonBody[List[Book]])
    val booksListingServerEndpoint: ServerEndpoint[Any, IO] =
        booksListing.serverLogicSuccess(_ => IO.pure(Library.books))

    val apiEndpoints: List[ServerEndpoint[Any, IO]] =
        List(helloServerEndpoint, booksListingServerEndpoint)

    val docEndpoints: List[ServerEndpoint[Any, IO]] = SwaggerInterpreter()
        .fromServerEndpoints[IO](apiEndpoints, "github-webhook-listener", "1.0.0")

    val all: List[ServerEndpoint[Any, IO]] = apiEndpoints ++ docEndpoints

object Library:
    case class Author(name: String)
    case class Book(title: String, year: Int, author: Author)

    val books = List(
        Book("The Sorrows of Young Werther", 1774, Author("Johann Wolfgang von Goethe")),
        Book("On the Niemen", 1888, Author("Eliza Orzeszkowa")),
        Book("The Art of Computer Programming", 1968, Author("Donald Knuth")),
        Book("Pharaoh", 1897, Author("Boleslaw Prus"))
    )
