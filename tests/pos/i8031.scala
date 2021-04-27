object Example extends App {

  trait Has[A]

  trait ZIO[-R] {
    def provideLayer[R0, R1 <: Has[_]](
        layer: ZLayer[R0, R1]
    )(implicit ev: R1 <:< R): ZIO[R0] =
      ???
  }

  trait ZLayer[-RIn, +ROut <: Has[_]] {
    def ++[RIn2, ROut1 >: ROut <: Has[_], ROut2 <: Has[_]](
        that: ZLayer[RIn2, ROut2]
    ): ZLayer[RIn with RIn2, ROut1 with ROut2] = ???
  }

  trait RandomService
  trait SizedService

  type Random = Has[RandomService]
  type Sized = Has[SizedService]

  def random: ZLayer[Random, Random] = ???
  def sized: ZLayer[Any, Sized] = ???

  lazy val zio: ZIO[Random with Sized] = ???

  // Okay on Scala 2, does not compile on Dotty
  lazy val eliminated: ZIO[Random] =
    zio.provideLayer(random ++ sized)

  // Compiles on Dotty with an explicit type annotation
  lazy val eliminated2: ZIO[Random] =
    zio.provideLayer[Random, Random with Sized](random ++ sized)

  println("It compiles!")
}
