package bar {
  trait M[F[_]]
  class S[XS[_] <: M[XS], A](val x: XS[A])
  object S {
    def apply[X[_] <: M[X], A](x: X[A]): S[X, A] = S[X, A](x)
    def unapply[X[_] <: M[X], A](p: S[X, A]): S[X, A] = S(p.x)
  }
}



