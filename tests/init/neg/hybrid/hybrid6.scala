class Foo {
  val bar = new Bar(this)
  var x: bar.Inner = new bar.Inner

  val list = List(1, 2, 3)    // error
}

class Bar(val _foo: Foo) {
  class Inner {
    val x = g
    val len = x.size
  }

  def g = _foo.list
}