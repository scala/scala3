trait NumericDate
trait JWSDecoded[H]

trait StandardHeaderWrite[H]:
  def setAlgorithm(header: H, algorithm: Algorithm): H

object StandardHeaderWrite:
  def apply[H](using sh: StandardHeaderWrite[H]): StandardHeaderWrite[H] = ???
  // unused - required to reproduce
  def apply[H](setAlg: (H, Algorithm) => H): StandardHeaderWrite[H] = ???

final case class JWK(algorithm: Option[Algorithm])
sealed trait Algorithm

def Test[F[_], H](key: JWK, header: H)(using StandardHeaderWrite[H]) = {
  key.algorithm
    .map(StandardHeaderWrite[H].setAlgorithm(header, _))
    .getOrElse(header)
}
