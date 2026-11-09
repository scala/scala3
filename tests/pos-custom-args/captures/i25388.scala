import scala.language.experimental.safe

case class A(i: Int) extends AnyVal:
  def addOne: A = A(i + 1)

def test: Int =
  val a = A(1)
  val b = a.addOne
  b.i
