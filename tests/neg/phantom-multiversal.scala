
class BooFunDef1 {
  import Universe1._
  import UniverseA._

  fun1(one, two)
  fun1(one, b) // error
  fun1(b, a) // error // error

  funA(a, b)
  funA(a, one) // error
  funA(two, one) // error // error

  funMulti(a, one, 42)
  funMulti(a, b, 42) // error
  funMulti(one, two, one) // error // error

  def fun1(x: One, y: Two) = ???
  def funA(k: A, l: B) = ???
  def funMulti(k: A, x: One, i: Int) = ???
}

object Universe1 extends Phantom {
  type One = this.Any
  type Two <: One
  def one: One = assume
  def two: Two = assume
}

object UniverseA extends Phantom {
  type A = this.Any
  type B <: A
  def a: A = assume
  def b: B = assume
}
