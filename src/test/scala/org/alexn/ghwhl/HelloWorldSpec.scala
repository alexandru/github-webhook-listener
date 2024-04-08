//package org.alexn.hook
//
//import cats.effect.IO
//import munit.CatsEffectSuite
//import org.alexn.hook.Endpoints.*
//import sttp.client3.{UriContext, basicRequest}
//import sttp.client3.testing.SttpBackendStub
//import sttp.tapir.integ.cats.effect.CatsMonadError
//import sttp.tapir.server.stub.TapirStubInterpreter
//import io.circe.generic.auto.*
//import sttp.client3.circe.*
//import org.alexn.hook.Library.Book
//import org.alexn.hook.Library.books
//
//class HelloWorldSpec extends CatsEffectSuite:
//
//    test("return hello world"):
//        // given
//        val backendStub = TapirStubInterpreter(SttpBackendStub(new CatsMonadError[IO]()))
//            .whenServerEndpointRunLogic(helloServerEndpoint)
//            .backend()
//
//        // when
//        val response = basicRequest
//            .get(uri"http://test.com/hello?name=adam")
//            .send(backendStub)
//
//        // then
//        assertIO(response.map(_.body), Right("Hello adam"))
//
//    test("list available books"):
//        // given
//        val backendStub = TapirStubInterpreter(SttpBackendStub(new CatsMonadError[IO]()))
//            .whenServerEndpointRunLogic(booksListingServerEndpoint)
//            .backend()
//
//        // when
//        val response = basicRequest
//            .get(uri"http://test.com/books/list/all")
//            .response(asJson[List[Book]])
//            .send(backendStub)
//            .map(_.body)
//
//        // then
//        assertIO(response, Right(books))
//
//end HelloWorldSpec
