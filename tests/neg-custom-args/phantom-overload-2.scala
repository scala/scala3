
class phantomOverload2 {
  import Boo._

  def foo1() = ???
  def foo1(x: A) = ??? // error
  def foo1(x1: B)(x2: N) = ??? // error

  def foo2(x1: Int, x2: A) = ???
  def foo2(x1: A)(x2: Int) = ??? // error
  def foo2(x1: N)(x2: A)(x3: Int) = ??? // error

  def foo3(x1: Int, x2: A) = ???
  def foo3(x1: Int, x2: A)(x3: A) = ??? // error
}

object Boo extends Phantom {
  type A <: this.Any
  type B <: this.Any
  type N = this.Nothing
}
