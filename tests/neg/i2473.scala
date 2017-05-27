trait Foo { trait Inner }
trait Bar { foo: Foo =>
  type Inner <: foo.Inner // error: type Inner cannot have the same name as trait Inner in trait Foo -- class definitions cannot be overridden
}
trait Baz { baz: Foo =>
  class Inner // error: class Inner cannot have the same name as trait Inner in trait Foo -- class definitions cannot be overridden
}
