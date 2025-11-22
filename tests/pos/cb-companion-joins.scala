//> using options -source:future
import language.experimental.modularity

trait M[Self]:
  extension (x: Self) def combine (y: Self): String
  def unit: Self

trait Num[Self]:
  def zero: Self

object Test1:
  trait X extends M[X]
  trait Y extends M[Y]

object Test2:
  trait A[X: Num]:
    X.zero
  trait B[X: {M, Num}]:
    X.unit
    X.zero

object Test3:

  trait A:
    type X: M
    X.unit

  trait B:
    type X: Num
    X.zero

  trait C extends A, B:
    X.zero
    X.unit

  class AA[Y: M] extends A:
    type X = Y
    X.unit
    Y.unit

  class CC[Y: {M, Num}] extends C:
    type X = Y
    X.zero
    X.unit
    Y.zero
    Y.unit
