import dotty.source.position.LineNumber
import dotty.source.position.SourcePath
object Test {
  def main(args: Array[String]): Unit = {
    println(SourcePath.thisSource)
    println(LineNumber.thisLine)
    println()
    println(implicitly[SourcePath])
    println(implicitly[LineNumber])
    println()
    printSourcePath()
    printPosition()
    println()
    printPosition(

    )
    println(new Foo)
    println()
    printPositionWithDefault2()()
    printPositionWithDefault2()(new LineNumber(4))
  }

  def printPosition()(implicit pos: LineNumber): Unit = println(pos)
  def printSourcePath()(implicit src: SourcePath): Unit = println(src)

  def printPositionWithDefault2()(pos: LineNumber = implicitly[LineNumber]): Unit = println(pos)

  class Foo(implicit pos: LineNumber) {
    override def toString: String = s"Foo<$pos>"
  }
}
