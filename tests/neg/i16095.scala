package x

import scala.annotation.*
import scala.concurrent.*

@capability
class CpsTransform[F[_]] {
     def await[T](ft: F[T]): { this } T = ???
}

inline def cpsAsync[F[_]] =
  Test.InfernAsyncArg
object Test {

  class InfernAsyncArg[F[_]] {
      def apply[A](expr: CpsTransform[F] ?=> A): F[A] = ???
  }

  def asyncPlus[F[_]](a:Int, b:F[Int])(using cps: CpsTransform[F]): { cps } Int =
    a + cps.await(b)

  def testExample1Future(): Unit =
     val fr = cpsAsync[Future] {
        val y = asyncPlus(1,Future successful 2)
        y+1
     }
     val r = Await.result(fr)
     assert(r == Success(3))

}