class Base
class Sub extends Base

trait A[+T] {
  def get(f: T => Boolean): Unit = {}
}

trait B extends A[Base] {
  override def get(f: Base => Boolean): Unit = f(new Base)
}

class C extends B with A[Sub] // error: Name clash between inherited members:

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    c.get((x: Sub) => true) // ClassCastException: Base cannot be cast to Sub
  }
}