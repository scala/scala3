
import scala.Tuple.IsMappedBy

object Test {

  def test[F[_], T <: Tuple: IsMappedBy[F]]: Unit = ()

  test[[X] =>> X, Unit]
  test[[X] =>> X, (Int, Long)]

  test[List, Unit]
  test[List, (List[Int], List[Long])]

  trait A[+X]
  trait B[-X]
  trait C[X]

  test[A, Unit]
  test[A, (A[Int], A[Long])]

  test[B, Unit]
  test[B, (B[Int], B[Long])]

  test[C, Unit]
  test[C, (C[Int], C[Long])]

}
