object Test {

  trait ZStream[-R, +E, +A]

  object ZStream {
    def empty: ZStream[Any, Nothing, Nothing] =
      ???
  }

  trait Gen[-R, +A](sample: ZStream[R, Nothing, A])

  def fromIterable[R, A](
    as: Iterable[A],
    shrinker: (A => ZStream[R, Nothing, A]) = (_: A) => ZStream.empty
  ): Gen[R, A] = ???
}