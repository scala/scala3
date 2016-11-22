
object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(new Foo)
  }

}

class Foo {
  override def toString: String = "Foo.toString"
}
