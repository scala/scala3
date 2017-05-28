trait Foo { trait Inner }
trait Bar { foo: Foo =>
  type Inner <: foo.Inner // error
}
trait Baz { baz: Foo =>
  class Inner // error
}
