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

object Test2 {
  class A
  class SubA(x: Int) extends A
  trait TA extends A
  trait TSubA extends SubA(2) // error: trait TSubA may not call constructor of class SubA


  class Foo extends TA with TSubA // error: missing argument for parameter x of constructor SubA:
}
