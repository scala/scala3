class OtherC[A, B, C <: B, D <: C]

trait crash {
  type OtherT[A, B, C <: B, D <: C]

  def indexK[F[X, Y <: X]]: F[Any, Any] = ???

  def res: OtherT[Any, Any, Any, Any] = indexK

  def res2: OtherC[Any, Any, Any, Any] = indexK
}
