
class Fun {
  def a = (x1: Int, x2: Boo.B) => x1 // error: Int and Boo.B are in different universes. They cannot be combined in function arguments.
  def b: (Int, Boo.B) => Unit = (x1, x2) => x1 // error: Int and Boo.B are in different universes. They cannot be combined in function arguments.

  def c = (x1: Foo.F, x2: Boo.B) => x1 // error: Foo.F and Boo.B are in different universes. They cannot be combined in function arguments.
  def d: (Foo.F, Boo.B) => Unit = (x1, x2) => x1 // error: Foo.F and Boo.B are in different universes. They cannot be combined in function arguments.
}

object Boo extends Phantom {
  type B <: this.Any
}

object Foo extends Phantom {
  type F <: this.Any
}
