object Example extends App {

  trait ZSink[-R, +E, +A0, -A, +B] {
    def flatMap[R1 <: R, E1 >: E, A00 >: A0, A1 <: A, C](
      f: B => ZSink[R1, E1, A00, A1, C]
    )(implicit ev: A00 =:= A1, e2: A1 =:= A00): ZSink[R1, E1, A00, A1, C] =
      ???
  }

  object ZSink {
    def identity[A]: ZSink[Any, Unit, Nothing, A, A] = ???
    def succeed[A, B](b: B): ZSink[Any, Nothing, A, A, B] = ???
  }

  // cannot prove that Int =:= Nothing
  ZSink.identity[Int].flatMap(n => ZSink.succeed[Int, String](n.toString))
}