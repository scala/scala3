import Ordering.Implicits.*

class A {
  import Predef.implicitly as ?

  ?[Ordering[List[Int]]]
  ?[Ordering[IndexedSeq[(Int, String)]]]
  ?[Ordering[Seq[Seq[Int]]]]
}
