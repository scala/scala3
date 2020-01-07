object Example extends App {

  trait ZSink[-R, +E, +A0, -A, +B] {
    def orElse[R1 <: R, E1, A00 >: A0, A1 <: A, C](
      that: ZSink[R1, E1, A00, A1, C]
    )(implicit ev: A1 =:= A00): ZSink[R1, E1, A00, A1, Either[B, C]] =
      ???
  }

  object ZSink {
    def identity[A]: ZSink[Any, Unit, Nothing, A, A] = ???
    def fail[E](e: E): ZSink[Any, E, Nothing, Any, Nothing] = ???
  }

  // compiles
  val a: ZSink[Any, String, Int, Int, Either[Int, Nothing]] =
    ZSink.identity[Int].orElse(ZSink.fail("Ouch"))

  // cannot prove that Int =:= Nothing
  ZSink.identity[Int].orElse(ZSink.fail("Ouch"))
}