abstract class A[T] {
  def foo: T
}
class B extends A[given Int => Int] {
  // No bridge needed for foo$direct
  def foo: given Int => Int = 1
}

abstract class X[T] extends A[given T => T] {
  def foo: given T => T
}

class Y extends X[Int] {
  def foo: given Int => Int = 1
}

object Test {
  def check(expected: Set[String], cls: Class[_]): Unit = {
    val actual = cls.getMethods.filter(_.getName.startsWith("foo")).map(_.toString).toSet
    assert(expected == actual, s"[$cls] expected: ${expected}\nactual: $actual")
  }

  def main(args: Array[String]): Unit = {
    val expectedB = Set(
      "public scala.Function1 B.foo()", // user-written method
      "public int B.foo$direct(int)", // shortcut added by ShortcutImplicits
      "public java.lang.Object B.foo()" // bridge to A#foo
    )
    val expectedX = Set(
      "public abstract scala.Function1 X.foo()", // user-written method
      "public abstract java.lang.Object X.foo$direct(java.lang.Object)", // shortcut
      "public abstract java.lang.Object A.foo()" // Inherited from A
    )
    val expectedY = Set(
      "public scala.Function1 Y.foo()", // user-written method
      "public java.lang.Object Y.foo()", // Bridge to A#foo
      "public int Y.foo$direct(int)", // shortcut
      "public java.lang.Object Y.foo$direct(java.lang.Object)", // bridge to X#foo$direct
    )

    check(expectedB, classOf[B])
    check(expectedX, classOf[X[_]])
    check(expectedY, classOf[Y])
  }
}
