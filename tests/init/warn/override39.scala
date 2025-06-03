abstract class Parent extends Product {
  val a = productArity
  val b = g

  def g: Int = productArity
}

case class Child(x: Int) extends Parent {
  val m = 10                  // warn
  def productArity: Int = m
}