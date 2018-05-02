import scala.annotation.filled

class Foo {
  val bar = new Bar(this)  // error
  new bar.Inner            // error

  new this.Inner           // error, as Inner access `this.list`

  val list = List(1, 2, 3)

  val inner: Inner @filled = new this.Inner // ok, `list` is instantiated

  val name = "good"

  class Inner {
    val len = list.size      // error: create new instance from line 5
  }
}

class Bar(val foo: Partial[Foo]) {
  val inner = new foo.Inner   // error

  class Inner {
    val len = inner.len
  }
}