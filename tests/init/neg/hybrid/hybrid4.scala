class Foo {
  class Inner {
    val len = list.size
  }

  val bar: Bar = new Bar(this)
  val list = List(1, 2, 3)       // error
}

class Bar(val _foo: Foo) {
  val inner = new _foo.Inner
}