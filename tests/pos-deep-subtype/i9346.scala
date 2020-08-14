trait Foo {
  type Repr[+O] <: Foo {
    type Repr[+OO] = Foo.this.Repr[OO]
  }

  def foo[T](f: Repr[T]): f.Repr[T] = ???
}