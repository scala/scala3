class Vals:
  val l = List(1)
  val ll = l :: List(1,2,3)

class A:
  def msg(a: Int, b: Int, c: Int) = "string" + a + "." + b + "." + c
  def integer: Int = 0
  def ex: this.type = this

@main
def Test: Unit =
  val a = A()
  val i = 123
  def f() = -1
  var x = a.msg(i, 0, a.integer)
  println(x)
  x = a.ex.msg(i, 0, a.ex.integer)
  println(x)
  x = a.msg(f(), 0, i)
  println(x)
