abstract class Base {
  def f: Int
  val a = f            // warn
}

class Derived extends Base {
  def f = g

  private def g: Int = 30
}

class Derived2 extends Base {
  val b = 30           // warn
  def f = g

  def g: Int = b + a
}