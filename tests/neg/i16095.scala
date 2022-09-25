package x

import scala.concurrent.*

def cpsAsync[F[_]] =
  Test.InfernAsyncArg

object Test {
  class InfernAsyncArg[F[_]] {
    def apply[A](): F[A] = ???
  }
  object InfernAsyncArg

  def testExample1Future(): Unit =
    val fr = cpsAsync[Future]() // error
}