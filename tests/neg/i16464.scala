
implicit final class SomeOps(e: Int) extends AnyVal:
  def -(other: Seq[Int]) = List(1)
  def -(other: Seq[Long]) = List(2) // error: double definition

def main(): Unit = 1 - Seq.empty[Int]
