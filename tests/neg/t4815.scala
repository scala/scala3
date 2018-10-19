class Test {
  def foo = {
    final def bar = 1 // error: local def may not be final
    final val v = 42 // error: local val may not be final
    final var v2 = 100 // error: local var may not be final
    final type T = Int // error: local type def may not be final
  }

  {
    final def foo(x: Int) = x // error: local def may not be final
  }

  final def foo2(x: Int) = x // ok: final allowed in class field

  object Foo {
    final def foo(x: Int) = x // ok, but redundant
  }

  abstract class Bar {
    def foo: Int
  }

  val x = new Bar {
    override final def foo = 42 // ok: def is a field
  }
}
