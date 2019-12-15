class Foo {
  val bar = new Bar(this)
  var x: bar.Inner = new bar.Inner

  class Inner {
    val len = list.size   // error
  }

  val list = List(1, 2, 3)
}

class Bar(val _foo: Foo) {
  class Inner {
    val x = g
    val len = x.len
  }

  def g = new _foo.Inner
}