trait Foo[+A] {
  type Repr[+O] <: Foo[O] {
    type Repr[+OO] = Foo.this.Repr[OO]
  }

  def foo: Repr[A]

  def bar: Repr[A] = this.foo.foo
}
