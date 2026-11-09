package example

case class A(x: Int) extends AnyVal

object A {
  val A1 = A(1)
  val A2 = A(2)
}

case class B(b: A.A1.type | A.A2.type)
