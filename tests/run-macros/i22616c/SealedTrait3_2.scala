sealed trait SealedTrait3[+A, +B]
object SealedTrait3 {
  final case class AB1[+B, +A](a: B, b: A) extends SealedTrait3[B, A]
  final case class AB2[+C, +D](a: C, b: D) extends SealedTrait3[D, C]
  final case class A[+T](a: T) extends SealedTrait3[T, Nothing]
  @caseName("_B_") final case class B[+T](b: T) extends SealedTrait3[Nothing, T]
  case object Neither extends SealedTrait3[Nothing, Nothing]
}
