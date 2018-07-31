class A {
  class Foo {
    transparent def inlineMeth: Unit = {
      new Bar
    }
  }
  class Bar
}

class B extends A {
  (new Foo).inlineMeth
}
