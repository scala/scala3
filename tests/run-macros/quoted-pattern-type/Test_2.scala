object Test {
  import Lib.*

  def main(args: Array[String]): Unit = {
    foo(true)
    foo(4)
    foo { println("hello"); "world" }
    foo { println("world"); "hello" }
    foo(Some(5))
  }
}
