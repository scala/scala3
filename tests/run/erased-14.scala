//> using options -language:experimental.erasedDefinitions

object Test {

  def main(args: Array[String]): Unit = {
    new Foo
  }

}

class Foo {
  erased val x: Int = {
    println("x")
    42
  }
  println("Foo")
}
