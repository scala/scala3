import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println((new Foo): Object)
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
