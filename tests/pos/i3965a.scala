trait SortedSet[A] extends SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A, +CC[X] <: SortedSet[X], +C <: SortedSetOps[A, CC, C]]

class TreeSet[A]
  extends SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]

class Test {
  def optionSequence1[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, CC[X]], A : Ordering](xs: CC[A]): Unit = ()

  def test(xs2: TreeSet[String]) = {
    optionSequence1(xs2)
  }
}
