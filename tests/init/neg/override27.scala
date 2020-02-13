abstract class Base {
  def f: Int
  val a = f            // error
}

class Derived extends Base {
  def f = g

  private def g: Int = 30
}

class Derived2 extends Base {
  val b = 30           // error
  def f = g

  def g: Int = b + a
}