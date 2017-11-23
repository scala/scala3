
class phantomOverload2 {
  import Boo._

  def foo1() = ???
  def foo1(unused x: A) = ??? // error
  def foo1(unused x1: B)(unused x2: N) = ??? // error

  def foo2(x1: Int)(unused x2: A) = ???
  def foo2(unused x1: A)(x2: Int) = ??? // error
  def foo2(unused x1: N)(unused x2: A)(x3: Int) = ??? // error

  def foo3(unused x1: Int, x2: A) = ???
  def foo3(unused x1: Int, x2: A)(unused x3: A) = ??? // error
}

object Boo extends Phantom {
  type A <: this.Any
  type B <: this.Any
  type N = this.Nothing
}
