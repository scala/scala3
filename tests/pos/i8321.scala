
object Test:
  inline def concat[A <: Tuple, B <: Tuple](
      a: Option[A],
      b: Option[B]
  ): Option[Tuple.Concat[A, B]] =
    a.zip(b).map(_ ++ _)

  concat(Some(1, 2), Some(3, 4))
