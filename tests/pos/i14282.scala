trait Foo[A] {
  inline def foo(): Unit
}

inline given FooA[A]: Foo[A] with {
  inline def foo(): Unit = println()
}
def test1 = FooA.foo()

inline given FooInt: Foo[Int] with {
  inline def foo(): Unit = println()
}
def test2 = FooInt.foo()
