import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(new Foo)
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
