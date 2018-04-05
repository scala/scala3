class Foo[U] { self : Array[U] & Nothing =>
  val s = self(0) // error: Array type conflict with Foo
}
