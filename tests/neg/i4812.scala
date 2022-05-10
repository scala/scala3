// scalac: -Werror
object Test:
  var prev: Any = _

  def test[T](x: T): T =
    class A(val elem: (T, Boolean))
    prev match
      case prev: A => // error: the type test for A cannot be checked at runtime
        prev.elem._1
      case _ =>
        prev = new A((x, true))
        x

  def test2[T](x: T): T =
    abstract class Parent(_elem: T) { def elem: T = _elem }
    class A extends Parent(x)
    prev match
      case prev: A => // error: the type test for A cannot be checked at runtime
        prev.elem
      case _ =>
        prev = new A
        x

  def test3[T](x: T): T =
    class Holder(val elem: T)
    class A(val holder: Holder)
    prev match
      case prev: A => // error: the type test for A cannot be checked at runtime
        prev.holder.elem
      case _ =>
        prev = new A(new Holder(x))
        x

  def test4[T](x: T): T =
    class Holder(val elem: (Int, (Unit, (T, Boolean))))
    class A { var holder: Holder = null }
    prev match
      case prev: A => // error: the type test for A cannot be checked at runtime
        prev.holder.elem._2._2._1
      case _ =>
        val a = new A
        a.holder = new Holder((42, ((), (x, true))))
        prev = a
        x

  class Foo[U]:
    def test5(x: U): U =
      class A(val elem: U)
      prev match
        case prev: A => // error: the type test for A cannot be checked at runtime
          prev.elem
        case _ =>
          prev = new A(x)
          x

  def test6[T](x: T): T =
    class Foo { var bar: Bar = null }
    class Bar { var foo: Foo = null; var elem: T = _ }
    prev match
      case prev: Foo => // error: the type test for A cannot be checked at runtime
        prev.bar.elem
      case _ =>
        val foo = new Foo
        val bar = new Bar
        bar.elem = x
        foo.bar = bar
        prev = foo
        x

  def main(args: Array[String]): Unit =
    test(1)
    val x: String = test("") // was: ClassCastException: java.lang.Integer cannot be cast to java.lang.String
