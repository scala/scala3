abstract class Parent extends Product {
  val a = productArity
  val b = g

  def g: Int = productArity     // ok
}

case class Child(x: Int) extends Parent {
  val m = 10
  def productArity: Int = m         // error // error
}