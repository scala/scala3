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

  class CpsTransform[F[_]] extends caps.SharedCapability {
     def await[T](ft: F[T]): T^{ this } = ???
  }

  transparent inline def cpsAsync[F[_]](using m:CpsMonad[F]) =
    new Test.InfernAsyncArg

  class InfernAsyncArg[F[_],C](using am:CpsMonad.Aux[F,C]) {
      def apply[A](expr: (CpsTransform[F], C) ?=> A): F[A] = ???
  }

  class C(x: Int):
    def +(that: C): C = ???

  def asyncPlus[F[_]](a:C, b:F[C])(using cps: CpsTransform[F]): C^{ cps } =
    a + (cps.await(b).asInstanceOf[C])

  def testExample1Future(): Unit =
     val fr = cpsAsync[Future] {
        val y = asyncPlus(C(1),Future successful C(2)).asInstanceOf[Int]
        y+1
     }

}
