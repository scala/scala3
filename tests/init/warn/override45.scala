class Parent {
  lazy val a = 5 * 8 + 9
  lazy val b = f
  def f: Int = 20
}

class Child extends Parent {
  val x = a
  val y = b
  override def f: Int = z
  val z = 30              // warn
}