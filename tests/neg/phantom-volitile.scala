
class Foo {
  @volatile var foo1 = Boo.boo // error: var fields cannot have Phantom types
  @volatile val foo2 = Boo.boo // error: Phantom fields cannot be @volatile
}

object Boo extends Phantom {
  def boo = assume
}
