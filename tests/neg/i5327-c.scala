trait SortedSetOps[CC[X], C]
class TreeSet[A] extends SortedSetOps[TreeSet, TreeSet[A]]

class Test {
  def optionSequence1[CC[X] <: SortedSetOps[CC, CC[X]] , A](xs: CC[A]): Unit = ()

  def test(xs2: Int) = {
    optionSequence1(xs2) // error
  }
}
