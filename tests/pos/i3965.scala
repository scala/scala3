trait Iterable[+A] extends  IterableOps[A, Iterable, Iterable[A]]
trait IterableOps[+A, +CCop[_], +C]

trait SortedSet[A] extends Iterable[A] with SortedSetOps[A, SortedSet, SortedSet[A]]

trait SortedSetOps[A, +CCss[X] <: SortedSet[X], +C <: SortedSetOps[A, CCss, C]]

class TreeSet[A]
  extends SortedSet[A]
    with SortedSetOps[A, TreeSet, TreeSet[A]]

class Test {
  def optionSequence1[CCos[X] <: IterableOps[X, CCos, _], A](xs: CCos[Option[A]]): Option[CCos[A]] = ???
  def optionSequence1[CC[X] <: SortedSet[X] with SortedSetOps[X, CC, CC[X]], A : Ordering](xs: CC[Option[A]]): Option[CC[A]] = ???

  def test(xs2: TreeSet[Option[String]]) = {
    optionSequence1(xs2)
  }
}
