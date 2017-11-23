
class Foo {
  unused var foo = Boo.boo // error
}

object Boo extends Phantom {
  type A <: this.Any
  unused def boo: A = assume
}
