
class Foo {
  @volatile var foo = Boo.boo // error
}

object Boo extends Phantom {
  def boo = assume
}
