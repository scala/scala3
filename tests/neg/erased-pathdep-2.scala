// Could become a neg test if we had totality checking for erased arguments

object Test {

  type F >: Bar <: Foo

  class A erased (val f: F) {
    type F1 <: f.X // error
    type F2[Z <: f.X] // error
  }

}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
