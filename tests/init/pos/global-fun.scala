import scala.annotation.init

class C:
  def double(x: Int): Int = x * 2

object A:
  val f: C => Int  = foo(10)

  def foo(x: Int): C => Int =
    c => c.double(x)  // error


object B:
  var y = A.f(new C: @init.expose)
