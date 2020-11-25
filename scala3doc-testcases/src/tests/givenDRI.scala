package tests.givenDRI

trait A[T]
trait B[T]
trait C

given A[Int]

given A[String]

given A[Seq[String]]

given [T: A] => A[Option[T]]

given [T: B] => A[T]

given [C] => A[C]

given A[C]

given [S <: C] => A[S]

class R:
  def a = 1

given A[Int] as r:
  def a = 2

class S:
  class R:
    def a = 3

  given A[Int] as r:
    def a = 5