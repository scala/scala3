import scala.Tuple.IsMappedBy

object Test2 {
  def test0[F[_], T: IsMappedBy[F]]: Unit = () // error: Type argument T does not conform to upper bound Tuple
}
