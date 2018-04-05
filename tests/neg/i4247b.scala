class Foo[U] { self : Array[U] & Nothing =>
  self(0) = ??? // error: Array type conflict with Foo
}
