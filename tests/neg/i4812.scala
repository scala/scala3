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
    class A { var b: B = null }
    class B { var a: A = null; var elem: T = _ }
    prev match
      case prev: A => // error: the type test for A cannot be checked at runtime
        prev.b.elem
      case _ =>
        val a = new A
        val b = new B
        b.elem = x
        a.b = b
        prev = a
        x

  def test7[T](x: T): T =
    class A(val elem: T)
    prev match
      case prev: A @unchecked => prev.elem
      case _                  => prev = new A(x); x

  def test8[T](x: T): T =
    class A(val elem: T)
    val p = prev
    (p: @unchecked) match
      case prev: A => prev.elem
      case _       => prev = new A(x); x

  def test9 =
    trait A
    class B extends A
    val x: A = new B
    x match
      case x: B => x

  sealed class A
  var prevA: A = _
  def test10: A =
    val methodCallId = System.nanoTime()
    class B(val id: Long) extends A
    prevA match
      case x: B => // error: the type test for B cannot be checked at runtime
        x.ensuring(x.id == methodCallId, s"Method call id $methodCallId != ${x.id}")
      case _    =>
        val x = new B(methodCallId)
        prevA = x
        x

  def test11 =
    trait A
    trait B
    class C extends A with B
    val x: A = new C
    x match
      case x: B => x

  def test12 =
    class Foo
    class Bar
    val x: Foo | Bar = new Foo
    x.isInstanceOf[Foo]

  def main(args: Array[String]): Unit =
    test(1)
    val x: String = test("") // was: ClassCastException: java.lang.Integer cannot be cast to java.lang.String
