
class Foo {
  var foo = Boo.boo // error: var fields cannot have Phantom types
}

object Boo extends Phantom {
  def boo = assume
}
