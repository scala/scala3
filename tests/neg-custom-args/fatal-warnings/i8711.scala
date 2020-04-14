class A
class B
class C

object Test {
  def foo(x: A) = x match {
    case x: B => x // error: this case is unreachable since class A is not a subclass of class B
    case _ =>
  }

  def bar(x: A | B) = x match {
    case x: C => x // error
    case _ =>
  }
}
