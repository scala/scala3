import scala.compiletime.testing.typeCheckErrors

object Test {
  def main(args: Array[String]): Unit = {
    println(typeCheckErrors("enum Foo { case A }"))
  }
}
