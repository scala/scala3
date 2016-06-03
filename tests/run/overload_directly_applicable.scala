class A
class B

class C1 {
  def f(x: A): Unit = println("C1")
}
class C2 extends C1 {
  def f(x: B): Unit = println("C2")
}

object Test extends C2 {
  implicit def a2b(x: A): B = new B
  def main(args: Array[String]): Unit = {
    f(new A)
  }
}
