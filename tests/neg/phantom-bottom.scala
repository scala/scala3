
class BooFunDef1 {
  import Boo._

  def fun0(x: Foo): x.Y = Boo.nothing

  def fun1(x: Foo): x.Y = ??? // error
  def fun2(x: Foo): x.Y = null // error
  def fun3(x: Foo): x.Y = Boo2.nothing // error
}

class Foo {
  type Y <: Boo.BooAny
}

object Boo extends Phantom {
  type BooAny = this.Any
  type BooNothing = this.Nothing
  def nothing: BooNothing = assume
}

object Boo2 extends Phantom {
  type BooNothing2 = this.Nothing
  def nothing: BooNothing2 = assume
}