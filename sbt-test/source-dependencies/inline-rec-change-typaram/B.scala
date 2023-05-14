object B {

  inline def inlinedAny[F[_], T](x: F[T]): x.type = x

}
