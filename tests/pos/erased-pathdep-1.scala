//> using options -language:experimental.erasedDefinitions

object Test {

  fun1(new Bar)
  val _ = fun2(new Bar)
  val _ = fun3(new Bar)

  def fun1[F >: Bar <: Foo](erased f: F): f.X = null.asInstanceOf[f.X]
  def fun2[F >: Bar <: Foo](erased f: F)(erased bar: f.B): f.B = null.asInstanceOf[f.B]
  def fun3[F >: Bar <: Foo](erased f: F)(erased b: f.B): b.X = null.asInstanceOf[b.X]
}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
