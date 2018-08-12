class A {
  class Foo {
    rewrite def inlineMeth: Unit = {
      new Bar
    }
  }
  class Bar
}

class B extends A {
  (new Foo).inlineMeth
}
