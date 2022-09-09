sealed trait Domain[E]

final def splitBounds[E, D[X] <: Domain[X]](
    bounds: Seq[E],
  )( using domain: D[E]): Seq[E] =
      val newBounds: Seq[E] = ???
      splitBounds(newBounds) // does not compile
      splitBounds[E,D](newBounds) // does compile