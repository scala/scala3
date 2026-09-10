import scala.annotation.targetName

extension [A](l: List[A])
  def isSorted  : Boolean = ???

def readSortedList(): {l: List[Int] with l.isSorted} = ???

@targetName("minSortedList")
def min(l: List[Int] with l.isSorted): Int = l.head // error

def min(l: List[Int]): Int = l.min

@main def Test =
  val l: List[Int] with l.isSorted = readSortedList()
  val m1 = min(l)
