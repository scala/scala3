import scala.concurrent.Future

class Gen[+T] {
  def map[U](f: T => U): Gen[U] = ???
}

object Gen {
  def oneOf[T](t0: T, t1: T): Gen[T] = ??? // Compile with this line commented
  def oneOf[T](g0: Gen[T], g1: Gen[T]): Gen[T] = ???
}

class Arbitrary[T]

object Arbitrary {
  def arbitrary[T]: Gen[T] = ???

  def arbFuture[X]: Gen[Future[X]] =
    Gen.oneOf(arbitrary[Future[X]], arbitrary[Throwable].map(Future.failed))
}