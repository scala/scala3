trait Foo:
  class Bar:
    private[Foo] opaque type Baz = Int

  def foo: Bar#Baz

