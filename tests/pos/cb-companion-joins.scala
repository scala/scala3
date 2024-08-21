import language.experimental.modularity
import language.future

trait M[Self]:
  extension (x: Self) def combine (y: Self): String
  def unit: Self

trait Num[Self]:
  def zero: Self

trait A extends M[A]
trait B extends M[A]

trait AA:
  type X: M
trait BB:
  type X: Num
class CC[X1: {M, Num}] extends AA, BB:
  type X = X1
  X.zero
  X.unit
