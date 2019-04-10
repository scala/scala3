// Could become a neg test if we had totality checking for erased arguments

object Test {

  fun1(new Bar)
  fun2(new Bar)
  fun3(new Bar)

  def fun1[F >: Bar <: Foo] erased (f: F): f.X = null.asInstanceOf[f.X] // error // error
  def fun2[F >: Bar <: Foo] erased (f: F) erased (bar: f.B): f.B = null.asInstanceOf[f.B] // error // error // error
  def fun3[F >: Bar <: Foo] erased (f: F) erased (b: f.B): b.X = null.asInstanceOf[b.X] // error // error // error
}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
