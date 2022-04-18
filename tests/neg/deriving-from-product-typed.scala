import deriving.Mirror

case class A(x: Int, y: String)
case class B(a: Any, b: Any)
object A:
  def f = summon[Mirror.ProductOf[A]].fromProductTyped((1, 2)) // error
  def g = summon[Mirror.ProductOf[A]].fromTuple((1, 2)) // error
  def h = summon[Mirror.ProductOf[B]].fromProductTyped(A(1, ""))
  def i = summon[Mirror.ProductOf[A]].fromProductTyped(B(1, "")) // error

