class Foo
class Bar
object A:
  extension (foo: Foo) def meth(): Foo = foo
object B:
  extension (bar: Bar) def meth(): Bar = bar

import A.*
import B.*

val foo = new Foo
val _ = meth(foo)() // error // error
