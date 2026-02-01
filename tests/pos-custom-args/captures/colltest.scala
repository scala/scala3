import language.experimental.captureChecking
// Showing a problem with recursive references
object CollectionStrawMan5 {

  /** Base trait for generic collections */
  trait Iterable[+A] extends IterableLike[A] {
    def iterator: Iterator[A]^{this}
    def coll: Iterable[A]^{this} = this
  }

  trait IterableLike[+A]:
    def coll: Iterable[A]^{this}
    def partition(p: A => Boolean): Unit =
      val pn = Partition(coll, p)
      ()

  /** Concrete collection type: View */
  trait View[+A] extends Iterable[A] with IterableLike[A]

  case class Partition[A](val underlying: Iterable[A]^, p: A => Boolean) {

    class Partitioned(expected: Boolean) extends View[A] uses Partition.this:
      this: Partitioned^{Partition.this} =>
      def iterator: Iterator[A]^{this} =
        underlying.iterator.filter((x: A) => p(x) == expected)

    val left: Partitioned^{Partition.this} = Partitioned(true)
    val right: Partitioned^{Partition.this} = Partitioned(false)
  }
}