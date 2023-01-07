import scala.annotation.targetName

object test1:
  implicit final class SomeOps(e: Int) extends AnyVal:
    @targetName("a")
    def -(other: Seq[Int]) = List(1)
    @targetName("b")
    def -(other: Seq[Long]) = List(2)

  def main(): Unit = 1 - Seq.empty[Int]

object test2:
  implicit final class SomeOps(e: Int) extends AnyVal:
    @targetName("a")
    def -(other: Seq[Int]) = List(1)
    def -(other: Seq[Long]) = List(2)

  def main(): Unit = 1 - Seq.empty[Int]

object test3:
  implicit final class SomeOps(e: Int) extends AnyVal:
    def -(other: Seq[Int]) = List(1)
    @targetName("b")
    def -(other: Seq[Long]) = List(2)

  def main(): Unit = 1 - Seq.empty[Int]
