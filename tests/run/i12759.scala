package example {

  trait Foo {
    val (a2, a3) = ("", "")
    val (x1, x2, x3) = ("", "", "")
  }

  class A extends Foo
}

object Test {
  def main(args: Array[String]): Unit = {
    new example.A
  }
}
