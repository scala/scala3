object Test {

  fun1(new Bar)
  fun2(new Bar)
  fun3(new Bar)

  def fun1[F >: Bar <: Foo](unused f: F): f.X = null.asInstanceOf[f.X]
  def fun2[F >: Bar <: Foo](unused f: F)(unused bar: f.B): f.B = null.asInstanceOf[f.B]
  def fun3[F >: Bar <: Foo](unused f: F)(unused b: f.B): b.X = null.asInstanceOf[b.X]
}

class Foo {
  type X
  type B <: Bar
}

class Bar extends Foo {
  type X = String
}
