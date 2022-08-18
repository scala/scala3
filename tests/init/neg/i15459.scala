class Sup:
  val a = 10
  println(this)  // error

class Sub extends Sup:
  val b = 20

  override def toString() = "a = " + a + ", b = " + b
