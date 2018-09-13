class A {
  class Foo {
    inline def inlineMeth: Unit = new Bar
  }
  class Bar
}

class B extends A {
  (new Foo).inlineMeth
}
