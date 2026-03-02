class A:
  val x = "x"
  class B:
    println(x)
    class C:
      println(x)
    new C
  new B

object Test:
  def main(args: Array[String]): Unit =
    new A
