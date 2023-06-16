object Test:
  abstract class Base(implicit config: Int)
  case class A(x: Int)(implicit config: Int) extends Base

  val a: A = A(3)(using 5)
