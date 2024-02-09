object foo {
  object HelloGen {
    println("hello world")
  }
  val Hello = HelloGen
}

import foo.Hello

object Test {
  def main(args: Array[String]): Unit = Hello: Unit
}
