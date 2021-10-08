import container.ir

opaque type Foo = ir.Foo

object Foo:
  def bar(foo: Foo): Unit = {}

export Foo.*