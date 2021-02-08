object Test1:
  def consume(xs: List[Int], limit: Int): List[Int] = xs match
    case x :: xs1 if limit > 0 => consume(xs1, limit - x)
    case _ => xs

object Test2 {
  import scala.math.Numeric
  import scala.math.Numeric.Implicits.*
  import scala.math.Ordering.Implicits.*

  def consume[T: Numeric](xs: List[T], limit: T): List[T] =
    val zero = implicitly[Numeric[T]].zero
    xs match {
      case x :: xs1 if limit > zero => consume(xs1, limit - x)
      case _ => xs
    }
}

object math3:
  trait Ord[T]:
    extension (x: T) def > (t: T): Boolean = ???
    extension (x: T) def <= (t: T): Boolean = ???

  trait Numeric[T] extends Ord[T]:
    extension (x: T) def + (y: T): T = ???
    extension (x: T) def - (y: T): T = ???
    extension (x: Int) def numeric: T = ???
end math3

object Test3:
  import math3.Numeric
  import collection.immutable.Seq

  def consume[T: Numeric](xs: List[T], limit: T): List[T] = xs match
    case x :: xs1 if limit > 0.numeric => consume(xs1, limit - x)
    case _ => xs

  def consume[T: Numeric](xs: LazyList[T], limit: T): LazyList[T] = xs match
    case x #:: xs1 if limit > 0.numeric => consume(xs1, limit - x)
    case _ => xs

  def consume[T: Numeric](xs: Seq[T], limit: T): Seq[T] =
    def dropCount(it: Iterator[T], start: Int, limit: T): Int =
      if limit > 0.numeric && it.hasNext then dropCount(it, start + 1, limit - it.next)
      else start
    xs.drop(dropCount(xs.iterator, 0, limit))

  def consume[T: Numeric](xs: Iterable[T], limit: T): Iterable[T] =
    var sum = 0.numeric
    xs.dropWhile { x =>
      try sum <= limit finally sum += x
    }

  def consume2[T: Numeric](xs: Iterable[T], limit: T): Iterable[T] =
    consume2(LazyList.from(xs), limit)
end Test3

