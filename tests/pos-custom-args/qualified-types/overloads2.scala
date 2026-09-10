import scala.annotation.targetName

extension [A](l: List[A])
  def isSorted  : Boolean = ???

// Example 2 from OOPSLA 26
@targetName("minSortedList")
def min(l: List[Int] with l.isSorted): Int = l.head // error
def min(l: List[Int]): Int = l.min
def example(l: List[Int] with l.isSorted): Int = min(l)
