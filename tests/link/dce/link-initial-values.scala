
object Test {
  def main(args: Array[String]): Unit = {
    new Bar
  }
}


class Bar extends Foo {
}

trait Foo {
  val foo: Int = {
    System.out.println(43)
    43
  }
}
