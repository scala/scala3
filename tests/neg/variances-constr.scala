class C[+A] {

  private[this] var y: A = compiletime.uninitialized
  def getY: A = y

  class Inner(x: A) { // error A appears contravariantly
    y = x
  }
  class Inner2[B <: A](x: B) { // error A appears contravariantly
    y = x
  }
}

object Test {

  def main(args: Array[String]) = {
    val x = new C[String]
    val y: C[Any] = x
    val i = new y.Inner(1)
    val s: String = x.getY
    val i2 = new y.Inner2(1)
    val s2: String = x.getY
    println(s)
  }
}

