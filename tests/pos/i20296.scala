trait Foo

object Foo {
  inline def bar(): Foo =
    class InlinedFoo extends Foo {}
    new InlinedFoo

  inline def foo(): Foo =
    bar()
    class InlinedFoo extends Foo {}
    new InlinedFoo

  def Test: Foo = Foo.foo()
}
