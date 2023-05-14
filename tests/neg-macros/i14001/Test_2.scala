package test
trait B {
  case class C(i: Int)
}

object A extends B

object X {
  val z = Y.testStuff[A.C] // error: Expr cast exception
}
