abstract class A extends Product {
  var n = productArity
}

case class B(x: Int, y: String) extends A

case class C(x: Int) extends A {
  val y = 10                      // warn
  def productArity: Int = y
}