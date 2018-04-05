class Foo[U] { self : Array[U] & Nothing =>
  self.length // error: Array type conflict with Foo
}
