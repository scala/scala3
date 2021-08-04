sealed trait PartialOrdering
sealed trait Ordering extends PartialOrdering

object Ordering {
  def fromCompare(n: Int): Ordering = new Ordering {}
}

trait PartialOrd[-A] {
  def checkCompare(l: A, r: A): PartialOrdering
}

trait Ord[-A] extends PartialOrd[A] {
  def checkCompare(l: A, r: A): Ordering
}

object Ord {
  def fromScala[A](implicit ordering: scala.math.Ordering[A]): Ord[A] =
     (l: A, r: A) => Ordering.fromCompare(ordering.compare(l, r))
}

object Test {
  def main(args: Array[String]): Unit =
    val intOrd = Ord.fromScala[Int]
    intOrd.checkCompare(1, 3)
}
