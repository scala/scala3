trait SubtypeOf[A[_], B[_]]

object Test1 {
  def instance[F[_], G[a] >: F[a]]: SubtypeOf[F, G] = new SubtypeOf[F, G] {}

  val x: SubtypeOf[List, Seq] = instance
}

object Test2 {
  def instance[G[_], F[a] <: G[a]]: SubtypeOf[F, G] = new SubtypeOf[F, G] {}

  val x: SubtypeOf[List, Seq] = instance
}
