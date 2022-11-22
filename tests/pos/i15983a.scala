class OtherC[A, B, C <: B]

trait crash {
  type OtherT[A, B, C <: B]

  def indexK[F[_]]: F[Any] = ???

  def res: OtherT[Any, Any, Any] = indexK

  def res2: OtherC[Any, Any, Any] = indexK
}
