sealed trait Show[-A]
final case class Pure[-B](showB: B => String)       extends Show[B]
final case class Many[-C](showL: List[C] => String) extends Show[List[C]]

object Test:
  def meth[X](show: Show[X]): X => String = show match
    case Pure(showB) => showB
    case Many(showL) =>
      val res = (xs: List[String]) => xs.head.length.toString
      res // error: Found: List[String] => String Required: X => String where: X is a type in method meth with bounds <: List[C$1]

  def main(args: Array[String]): Unit =
    val show = Many((is: List[Int]) => (is.head + 1).toString)
    val fn   = meth(show)
    assert(fn(List(42)) == "43") // was: ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String
