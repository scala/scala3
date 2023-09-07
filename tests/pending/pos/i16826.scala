import language.experimental.captureChecking
class A
class B(a: {*} A)
class C(a: {*} A):
  def setB(b: {a} B): Unit = ???


def test(a1: {*} A)(b1: {a1} B) =
  val c = new C(a1)
  c.setB(b1)
