
import scala.Tuple.IsMappedBy

object Test {

  def test[F[_], T <: Tuple: IsMappedBy[F]]: Unit = ()

  test[[X] =>> X, EmptyTuple]
  test[[X] =>> X, (Int, Long)]

  test[List, EmptyTuple]
  test[List, (List[Int], List[Long])]

  trait A[+X]
  trait B[-X]
  trait C[X]

  test[A, EmptyTuple]
  test[A, (A[Int], A[Long])]

  test[B, EmptyTuple]
  test[B, (B[Int], B[Long])]

  test[C, EmptyTuple]
  test[C, (C[Int], C[Long])]

}
