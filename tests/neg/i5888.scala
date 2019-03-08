val x: String <:< Int = _.toInt  // error

abstract final class Foo { def f(x: Int): Int }

val foo: Foo = x => x  // error

val foo2: Foo = Predef.identity // error

