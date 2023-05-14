object B {

  inline def inlinedAny[F[X] >: List[X] <: List[X], T <: String](x: F[T]): x.type = x

}
