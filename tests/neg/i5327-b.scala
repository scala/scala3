trait T
trait SortedSetOps[CC[X], C]
class TreeSet[A] extends SortedSetOps[[X] => (TreeSet[X] & T), TreeSet[A] & T] // error

class Test {
  def optionSequence1[CC[X] <: (SortedSetOps[CC, CC[X]] & T) , A](xs: CC[A]): Unit = ()
  def test(xs2: TreeSet[String] & T) = {
    optionSequence1(xs2) // error
  }
}
