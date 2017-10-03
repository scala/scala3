object Test {

  type F >: Bar <: Foo

  class A(unused val f: F) {
    type F1 <: f.X
    type F2[Z <: f.X]
  }

}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
