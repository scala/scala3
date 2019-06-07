
import Positioned._

object Test {
  def main(args: Array[String]): Unit = {
    def printPos[T](x: Positioned[T]) = {
      val pos = x.position
      println(s"${x.value} columns:${pos.startColumn}-${pos.endColumn} lines:${pos.startLine}-${pos.endLine}")
    }
    printPos(Positioned(0))
    printPos(10)
    printPos(4530)
    printPos(Positioned("acbvasdfa"))
    printPos("acbvasdfa")
    printPos(
      """a
        |b""".stripMargin: String)
    printPos(new Foo)
  }
  class Foo {
    override def toString: String = "Foo"
  }
}
