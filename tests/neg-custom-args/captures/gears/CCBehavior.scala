import language.experimental.captureChecking

import gears.async.AsyncOperations.*
import gears.async.default.given
import gears.async.{Async, AsyncSupport, Future, uninterruptible}

import java.util.concurrent.CancellationException
import scala.annotation.capability
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Success
import scala.util.boundary
import gears.async.Channel
import gears.async.SyncChannel

type Result[+T, +E] = Either[E, T]
object Result:
  opaque type Label[-T, -E] = boundary.Label[Result[T, E]]
  // ^ doesn't work?

  def apply[T, E](body: Label[T, E]^ ?=> T): Result[T, E] =
    boundary(Right(body))

  extension [U, E](r: Result[U, E])(using Label[Nothing, E]^)
    def ok: U = r match
      case Left(value)  => boundary.break(Left(value))
      case Right(value) => value

class CaptureCheckingBehavior extends munit.FunSuite:
  import Result.*
  import caps.use
  import scala.collection.mutable

  test("good") {
    // don't do this in real code! capturing Async.blocking's Async context across functions is hard to track
    Async.blocking: async ?=>
      def good1[T, E](@use frs: List[Future[Result[T, E]]^]): Future[Result[List[T], E]]^{frs*, async} =
        Future: fut ?=>
          Result: ret ?=>
            frs.map(_.await.ok)

      def good2[T, E](@use rf: Result[Future[T]^, E]): Future[Result[T, E]]^{rf*, async} =
        Future:
          Result:
            rf.ok.await // OK, Future argument has type Result[T]

      def useless4[T, E](fr: Future[Result[T, E]]^) =
        fr.await.map(Future(_))
  }

  // test("bad - collectors") {
  //   val futs: Seq[Future[Int]^] = Async.blocking: async ?=>
  //     val fs: Seq[Future[Int]^{async}] = (0 to 10).map(i => Future { i })
  //     fs
  //   Async.blocking:
  //     futs.awaitAll // should not compile
  // }

  test("future withResolver capturing") {
    class File() extends caps.Capability:
      def close() = ()
      def read(callback: Int => Unit) = ()
    val f = File()
    val read  = Future.withResolver[Int, caps.CapSet^{f}]: r =>
      f.read(r.resolve)
      r.onCancel(f.close)
  }

  test("awaitAll/awaitFirst") {
    trait File extends caps.Capability:
      def readFut(): Future[Int]^{this}
    object File:
      def open[T](filename: String)(body: File => T)(using Async): T = ???

    def readAll(@caps.use files: (File^)*) = files.map(_.readFut())

    Async.blocking:
      File.open("a.txt"): a =>
        File.open("b.txt"): b =>
          val futs = readAll(a, b)
          val allFut = Future(futs.awaitAll)
          allFut
            .await // uncomment to leak
  }

  // test("channel") {
  //   trait File extends caps.Capability:
  //     def read(): Int = ???
  //   Async.blocking:
  //     val ch = SyncChannel[File]()
  //     // Sender
  //     val sender = Future:
  //       val f = new File {}
  //       ch.send(f)
  //     val recv = Future:
  //       val f = ch.read().right.get
  //       f.read()
  // }

  test("very bad") {
    Async.blocking: async ?=>
      def fail3[T, E](fr: Future[Result[T, E]]^): Result[Any, Any] =
        Result: label ?=>
          Future: fut ?=>
            fr.await.ok // error, escaping label from Result

      // val fut = Future(Left(5))
      // val res = fail3(fut)
      // println(res.right.get.asInstanceOf[Future[Any]].awaitResult)
  }

  // test("bad") {
  //   Async.blocking: async ?=>
  //     def fail3[T, E](fr: Future[Result[T, E]]^): Result[Future[T]^{async}, E] =
  //       Result: label ?=>
  //         Future: fut ?=>
  //           fr.await.ok // error, escaping label from Result
  // }
