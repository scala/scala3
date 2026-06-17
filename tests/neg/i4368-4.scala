object Test4 {
  trait X[F[_]] {
    protected type A = F[B]
    protected type B
  }
  trait Y[F[_]] {
    protected type A
    protected type B = F[A]
  }

  trait Fix[F[_]] extends X[F] with Y[F] {
    type Result = A    // error: too deep
  }
}
