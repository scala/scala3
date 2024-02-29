// Showing a problem with recursive references
object CollectionStrawMan5 {

  /** Base trait for generic collections */
  trait Iterable[+A] extends IterableLike[A] {
    this: Iterable[A]^ =>
    def iterator: Iterator[A]^{this}
    def coll: Iterable[A]^{this} = this
  }

  trait IterableLike[+A]:
    this: IterableLike[A]^ =>
    def coll: Iterable[A]^{this}
    def partition(p: A => Boolean): Unit =
      val pn = Partition(coll, p)
      ()

  /** Concrete collection type: View */
  trait View[+A] extends Iterable[A] with IterableLike[A] {
    this: View[A]^ =>
  }

  case class Partition[A](val underlying: Iterable[A]^, p: A => Boolean) {
    self: Partition[A]^{underlying, p} =>

    class Partitioned(expected: Boolean) extends View[A]:
      this: Partitioned^{self} =>
      def iterator: Iterator[A]^{this} =
        underlying.iterator.filter((x: A) => p(x) == expected)

    val left: Partitioned^{self} = Partitioned(true)
    val right: Partitioned^{self} = Partitioned(false)
  }
}