import scala.Tuple.IsMappedBy

object Test1 {

  def test[F[_], T <: Tuple: IsMappedBy[F]]: Unit = ()

  test[List, (List[Int], Char)] // error
  test[List, (List[Int], Seq[Char])] // error
  test[Seq, (List[Int], Seq[Char])] // error
  test[List, Tuple] // error
  test[List, Nothing] // error

}
