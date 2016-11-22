package hello

import scala.scalajs.js

trait MyTrait {
  val x = 5
  def foo(y: Int) = x
}

object world extends js.JSApp with MyTrait {
  def main(): Unit = {
    println("hello dotty.js!")
    println(foo(4))
  }
}
