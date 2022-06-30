import scala.language.experimental.erasedDefinitions

class A:
  erased def x: String = "x".toString
  def foo(erased s: String): String = "foo"

@main
def Test: Unit =
  val a = A()
  // FIXME: coverage should not track erased arguments and statements
  // a.x
  // println(a.foo(a.x))
  println("foo")
