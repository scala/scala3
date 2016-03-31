package hello

import scala.annotation.tailrec

class Enclosing {
  class SomeData(val x: Int)

  def localDef(): Unit = {
    def foo(data: SomeData): Int = data.x

    @tailrec
    def test(i: Int, data: SomeData): Unit = {
      if (i != 0) {
        println(foo(data))
        test(i - 1, data)
      }
    }

    test(3, new SomeData(42))
  }
}

object world extends App {
  println("hello dotty!")
  new Enclosing().localDef()
}
