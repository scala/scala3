class HasFoo[T] {
  val x: Foo[T] = ???
}
class Foo[T] {
  def get(x: T): T = x
  def get2(x: T): Nothing = ???

  def foo1(x: T)(implicit ev: T): Nothing = ???
  def foo2(x: T)(implicit ev: T): T = ???
  def foo3[Dummy](x: T)(implicit ev: T): Nothing = ???
  def foo4[Dummy](x: T)(implicit ev: T): T = ???
}

object Test {

  def foo1[T](x: T)(implicit ev: T): Nothing = ???
  def foo2[T](x: T)(implicit ev: T): T = ???

  def test1: Unit = {
    implicit val ii: Int = 42

    foo1(10)
    foo2(10)
  }

  def hf[T]: HasFoo[T] = ???
  def test2: Unit = {
    implicit val ii: Int = 42

    hf.x.get(10)
    hf.x.get2(10)

    hf.x.foo1(10)
    hf.x.foo2(10)
    hf.x.foo3(10)
    hf.x.foo4(10)
  }
}
