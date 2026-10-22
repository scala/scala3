//> using options -Ykind-projector
object x {
  sealed trait ZIO[-R, +E, +A] extends ZIOVersionSpecific[R, E, A]
  // this trait is required for the error to surface
  trait ZIOVersionSpecific[-R, +E, +A]

  type IO[+E, +A] = ZIO[Any, E, A]
}

object example {

  import x.{IO, ZIO}

  trait Panic3[F[-_, +_, +_]] extends Root

  type Panic2[F[+_, +_]] = Panic3[λ[(`-R`, `+E`, `+A`) => F[E, A]]]

  trait Root
  object Root extends RootInstancesLowPriority7

  sealed trait RootInstancesLowPriority7 extends RootInstancesLowPriority12 {
    @inline implicit final def BIOZIO: Panic3[ZIO] = null
  }

  sealed trait RootInstancesLowPriority12 {
    @inline implicit final def Convert3To2[C[f[-_, +_, +_]] <: Root, FR[-_, +_, +_], R0](
      implicit BifunctorPlus: Root & C[FR] // `Root &` required due to https://github.com/lampepfl/dotty/issues/13986
    ): C[λ[(`-R`, `+E`, `+A`) => FR[R0, E, A]]] = {
      BifunctorPlus.asInstanceOf[C[λ[(`-R`, `+E`, `+A`) => FR[R0, E, A]]]]
    }
  }

  trait FreeMonad[F[+_, +_], +E, +A] {
    @inline def foldMap[G[+_, +_], E, A](fg: Interpreter[F, G])(implicit G: Panic2[G]): G[E, A] = null.asInstanceOf[G[E, A]]
  }

  // NOTE: variance on Interpreter parameters is required for the error, removing variance makes it disappear
  // type Interpreter[Source[+_, +_], Target[+_, +_]]
  type Interpreter[-Source[+_, +_], +Target[+_, +_]]

  sealed trait TestFreeChoice[+E, +A]

  @main def main: Unit = {
    println(
      implicitly[Panic2[IO]]
    )
    println(
      implicitly[Panic2[λ[(`+E`, `+A`) => ZIO[Any, E, A]]]]
    )

    val interpreter: Interpreter[TestFreeChoice, IO] = null.asInstanceOf[Interpreter[TestFreeChoice, IO]]
    val value: FreeMonad[TestFreeChoice, Nothing, Unit] = new FreeMonad[TestFreeChoice, Nothing, Unit] {}
    println(value.foldMap(interpreter))
  }

}
