object Test {

  def main(args: Array[String]): Unit = {
    new Foo
  }

}

class Foo {
  unused val x: Int = {
    println("x")
    42
  }
  println("Foo")
}
