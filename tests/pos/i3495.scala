class A

object Test {
  val x: A = new A()

  def y = x.getClass

  val z: Class[? <: A] = y

  1.getClass

}

