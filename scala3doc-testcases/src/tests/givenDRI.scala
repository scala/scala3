package tests.givenDRI

trait A[T]
trait B[T]
trait C

given A[Int]

given A[String]

given A[Seq[String]]

given [T: A] as A[Option[T]]

given [T: B] as A[T]

given [C] as A[C]

given A[C]

given [S <: C] as A[S]

class R:
  def a = 1

given R as A[Int]:
  def a = 2

class S:
  class R:
    def a = 3

  given R as A[Int]:
    def a = 5