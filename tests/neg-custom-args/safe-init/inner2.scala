import scala.annotation.filled

class Foo {
  val list = List(1, 2, 3)

  class Inner {
    val len = list.size
  }

  val bar: Bar @filled = new Bar(this)
  new bar.Inner            // error
}

import scala.annotation.partial

class Bar(val foo: Partial[Foo]) {
  val inner = new foo.Inner   // error

  class Inner {
    val x = new foo.Inner
    val len = x.len
  }
}