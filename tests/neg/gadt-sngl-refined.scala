trait Foo {
  type A >: Nothing <: Any
}

enum SUB[-A, +B]:
  case Refl[X]() extends SUB[X, X]

import SUB._

def test[X](foo: Foo, e: SUB[foo.type, Foo {type A <: X}], x: Any): X = e match
  case Refl() =>
    // From foo.type <:< Foo{type A <: X} we should not infer GADT constraints.
    x   // error

val foo = new Foo { type A = Nothing }

def unsound(x: Any): Nothing =
  test[Nothing](foo, Refl(), x)
