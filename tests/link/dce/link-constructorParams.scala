

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(new Foo(42).x)
  }
}

class Foo(x: Int) extends Bar(x)
class Bar(val x: Int)
