import scala.reflect.Selectable.reflectiveSelectable

trait Foo1 { val a: Int }
trait Foo2 { def a: Int }
trait Foo3 { var a: Int }

object Test {
  private class FooBar1 extends Foo1 { val a: Int = 10 }
  private class FooBar2 extends Foo2 { def a: Int = 10 }
  private class FooBar3 extends Foo3 { var a: Int = 10 }

  private class Bar1 { val a: Int = 10 }
  private class Bar2 { def a: Int = 10 }
  private class Bar3 { var a: Int = 10 }

  object TestStructuralVal {
    // This test is also an (abstracted) motivating example.

    // Consider one module upcasting all these instances to T. These casts are clearly well-typed.
    type T = {val a: Int}
    type T2 = {def a: Int}
    def upcast1(v: Foo1): T = v
    def upcast2(v: Foo2): T2 = v
    def upcast3(v: Foo3): T2 = v

    // These accesses are also clearly well-typed
    def consume(v: T) = v.a
    inline def consumeInl(v: T) = v.a
    def verify(v: T) = {
      assert(consume(v) == 10)
      assert(consumeInl(v) == 10)
      assert(v.a == 10)
    }

    def consume2(v: T2) = v.a
    inline def consumeInl2(v: T2) = v.a
    def verify2(v: T2) = {
      assert(consume2(v) == 10)
      assert(consumeInl2(v) == 10)
      assert(v.a == 10)
    }

    def test(): Unit = {
      // These calls are also clearly well-typed, hence can't be rejected.
      verify(upcast1(new Foo1 { val a = 10 }))
      verify2(upcast2(new Foo2 { val a = 10 }))
      verify2(upcast3(new Foo3 { var a = 10 }))
      // Ditto, so we must override access control to the class.
      verify(upcast1(new FooBar1))
      verify2(upcast2(new FooBar2))
      verify2(upcast3(new FooBar3))

      // Other testcases
      verify(new {val a = 10} : T)
      verify2(new {var a = 10} : T2)
      verify2(new {def a = 10} : T2)

      verify(new Bar1 : T)
      verify2(new Bar2 : T2)
      verify2(new Bar3 : T2)
    }
  }

  object TestStructuralDef {
    type T = {def a: Int}
    def upcast1(v: Foo1): T = v
    def upcast2(v: Foo2): T = v
    def upcast3(v: Foo3): T = v
    def consume(v: T) = v.a
    inline def consumeInl(v: T) = v.a
    def verify(v: T) = {
      assert(consume(v) == 10)
      assert(consumeInl(v) == 10)
      assert(v.a == 10)
    }

    def test(): Unit = {
      verify(upcast1(new Foo1 { val a = 10 }))
      verify(upcast2(new Foo2 { val a = 10 }))
      verify(upcast3(new Foo3 { var a = 10 }))

      verify(upcast1(new FooBar1))
      verify(upcast2(new FooBar2))
      verify(upcast3(new FooBar3))

      verify(new {val a = 10} : T)
      verify(new {var a = 10} : T)
      verify(new {def a = 10} : T)

      verify(new Bar1 : T)
      verify(new Bar2 : T)
      verify(new Bar3 : T)
    }
  }

  object TestStructuralVar {
    type T = {def a: Int; def a_=(x: Int): Unit}
    def upcast3(v: Foo3): T = v
    def consume(v: T) = v.a
    inline def consumeInl(v: T) = v.a
    def verify(v: T) = {
      assert(consume(v) == 10)
      assert(consumeInl(v) == 10)
      assert(v.a == 10)
      // Pending, per https://github.com/scala/scala3/issues/4528.
      // v.a = 11
      // assert(consume(v) == 11)
      // assert(consumeInl(v) == 11)
      // assert(v.a == 11)
    }

    def test(): Unit = {
      verify(upcast3(new Foo3 { var a = 10 }))
      verify(upcast3(new FooBar3))
      verify(new {var a = 10} : T)
      verify(new Bar3 : T)
    }
  }

  def main(args: Array[String]): Unit = {
    TestStructuralVal.test()
    TestStructuralDef.test()
    TestStructuralVar.test()
  }
}
