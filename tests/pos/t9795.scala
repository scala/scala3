case class A(v: Int)
class B(v: Int) extends A(v)

object Test {
  val a = new B(1)
  a match { case A(_) => 1 }
}
