package x

import scala.annotation.*
import scala.concurrent.*

trait CpsMonad[F[_]] {
  type Context
}

object CpsMonad {
  type Aux[F[_],C] = CpsMonad[F] { type Context = C }
  given CpsMonad[Future]()
}

@experimental
object Test {

  class CpsTransform[F[_]] extends caps.Capability {
     def await[T](ft: F[T]): T^{ this } = ???
  }

  transparent inline def cpsAsync[F[_]](using m:CpsMonad[F]) =
    new Test.InfernAsyncArg

  class InfernAsyncArg[F[_],C](using am:CpsMonad.Aux[F,C]) {
      def apply[A](expr: (CpsTransform[F], C) ?=> A): F[A] = ???
  }

  def asyncPlus[F[_]](a:Int, b:F[Int])(using cps: CpsTransform[F]): Int^{ cps } =
    a + (cps.await(b).asInstanceOf[Int])

  def testExample1Future(): Unit =
     val fr = cpsAsync[Future] {
        val y = asyncPlus(1,Future successful 2).asInstanceOf[Int]
        y+1
     }

}
