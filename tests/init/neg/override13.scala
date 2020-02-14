abstract class A {
  val x = f

  def f: Int
}

class B(val y: Int) extends A {
  def f: Int = y
}

class C extends B(5) {
  override val y: Int = 10  // error
}
