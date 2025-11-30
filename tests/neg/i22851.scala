object Test {
  def first[T <: Product](t: T)(using
      m: scala.deriving.Mirror.ProductOf[T] {
        type MirroredElemTypes <: NonEmptyTuple
      }
  ): Tuple.Head[m.MirroredElemTypes] = ???

  first(EmptyTuple) // error
  // MirroredElemTypes missmatch, expected: EmptyString, found: <: NonEmptyTuple.
}
