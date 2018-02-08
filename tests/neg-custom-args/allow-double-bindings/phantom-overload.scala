
class phantomOverload {
  import Boo._
  import Boo2._

  def foo1(): A = nothing
  def foo1(): B = nothing // error
  def foo1(): C = nothing2 // error
  def foo1(): N = nothing // error

  def foo2(x: A) = ???
  def foo2(x: A) = ??? // error
  def foo2(x: B) = ??? // error
  def foo2(x: C) = ??? // error
  def foo2(x: N) = ??? // error
}

object Boo extends Phantom {
  type A <: this.Any
  type B <: this.Any
  type N = this.Nothing
  def nothing: this.Nothing = assume
}

object Boo2 extends Phantom {
  type C <: this.Any
  def nothing2: this.Nothing = assume
}