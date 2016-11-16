
object Test {
  def main(args: Array[String]): Unit = {
    println(new Foo) // In this test case the standard lib is compiled separately.
  }

}

class Foo {
  override def toString: String = "Foo.toString"
}
