
object Test {
  def main(args: Array[String]): Unit = {
    println(new Foo)
  }
}

class Foo {
  override def toString: String = "foo"
}
