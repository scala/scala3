class X(val x: Int) extends AnyVal
class Y(val y: Object) extends AnyVal {
  override def toString = s"Y($y)"
}
class Z(val z: String) extends AnyVal {
  override def toString = s"Z($z)"
}
object Test {
  def main(args: Array[String]) = {
    println(a.toSet)
    println(b.toSet)
    println(c.toSet)
  }

  def a = new Array[X](2)
  def b = new Array[Y](3)
  def c = new Array[Z](4)
}