package covtest

class A:
  def msg(a: Int, b: Int, c: Int) = "string"
  def integer: Int = 0
  def ex: this.type = this

def test(): Unit =
  val a = A()
  val i = 123
  a.msg(i, 0, a.integer)
  a.ex.msg(i, 0, a.ex.integer)
