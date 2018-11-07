abstract class Base {
  def f: Int
  val a = f
}

class Derived extends Base {
  def f = g   // calling `g` should be OK

  private def g: Int = 30
}

class Derived2 extends Base {
  def f = g   // error // error: `g` is dynamic

  def g: Int = 30
}