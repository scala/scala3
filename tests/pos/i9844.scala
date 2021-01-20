object test1 with
  trait Foo[A]

  trait Baz[A]  {
    trait Bar {
      this: Foo[A] =>
      def bar(a: A): Unit
    }
  }

object test2 with

  trait Foo with
    private var f = "abc"

  trait Baz  {
    trait Bam with
      val f = 0
    trait Bar extends Bam {
      this: Foo =>
        val g = f
        val g1: Int = g
    }
  }

object test3 with
  object DetSkipOctree {
    sealed trait Leaf  [PL]
    sealed trait Branch[PL]
  }
  trait DetSkipOctree[PL]

  class Impl[PL] extends DetSkipOctree[PL] {
    final type Leaf = DetSkipOctree.Leaf[PL]

    protected trait LeftBranchImpl {
      this: DetSkipOctree.Branch[PL] =>

      def demoteLeaf(point: PL, leaf: Leaf): Unit = ???
    }
  }