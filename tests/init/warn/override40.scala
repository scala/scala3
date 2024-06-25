abstract class A {
  val a: Int
  def f: Int = 10 * a
}

class B extends A {
  this.getClass
  f
  val a = 20   // warn
}