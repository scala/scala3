import scala.annotation.targetName

implicit final class SomeOps(e: Int) extends AnyVal:
  @targetName("a")
  def -(other: Seq[Int]) = List(1)
  @targetName("b")
  def -(other: Seq[Long]) = List(2)

@main
def Test(): Unit = 1 - Seq.empty[Int]