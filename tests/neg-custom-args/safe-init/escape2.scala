class Foo {
  new Bar(this)           // error
  val id = 567
  val b = new Bar(this)   // error
}

class Bar(foo: Foo) {
  println(foo.id)
  println(foo.b)
}
