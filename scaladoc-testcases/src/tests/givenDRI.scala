package tests.givenDRI

trait A[T]
trait B[T]
trait C

given A[Int] with {}

given A[String] with {}

given A[Seq[String]] with {}

given [T: A]: A[Option[T]] with {}

given [T: B]: A[T] with {}

given [C]: A[C] with {}

given A[C] with {}

given [S <: C]: A[S] with {}

class R:
  def a = 1

given RR: A[Int] with
  def a = 2

class S:
  class R:
    def a = 3

  given R: A[Int] with
    def a = 5
