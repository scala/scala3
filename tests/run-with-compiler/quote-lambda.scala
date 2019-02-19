import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    '{ (x: Int) => ${'x} }
  }
}
