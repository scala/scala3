import scala.annotation.internal

object Test {

  def main(args: Array[String]): Unit = ()

  @internal.link.AssertReachable def foo() = System.out.println(42)

  foo()

}
