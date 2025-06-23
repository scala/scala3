//> using options -language:experimental.erasedDefinitions

// Could become a neg test if we had totality checking for erased arguments

object Test {

  type F >: Bar <: Foo

  class A(erased val f: F) {
    type F1 <: f.X // was error
    type F2[Z <: f.X] // was error
  }

}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
