import scala.annotation.init

class C:
  def double(x: Int): Int = x * 2

object A:
  val n: Int = 10
  val f: C => Int  = foo(n: @init.expose)

  def foo(x: Int): C => Int =
    c => c.double(x: @init.expose)


object B:
  var y = A.f(new C: @init.expose)
