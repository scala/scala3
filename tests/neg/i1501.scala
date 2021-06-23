class A {
  def foo: Int = 1
}

trait B extends A

abstract class D {
  def foo: Int
}

class C extends D with B // error: illegal trait inheritance
trait E extends D with B // error: illegal trait inheritance

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().foo)
  }
}
