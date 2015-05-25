package dotty.collections
package immutable

import annotation.unchecked.uncheckedVariance

trait Collection[+CC[X] <: Collection[CC, X], T] {
  def companion: CollectionCompanion[CC]
}

trait Iterable[T] extends Collection[Iterable, T] {
  def iterator: Iterator[T]
  override def companion: IterableCompanion[Iterable] = Iterable
}

trait Seq[T] extends Iterable[T] with Collection[Seq, T] {
  def apply(x: Int): T
  override def companion: IterableCompanion[Seq] = Seq
}

abstract class CollectionCompanion[+CC[X] <: Collection[CC, X]]

trait IterableImpls[CC[X]] {
  def fromIterator[T](it: Iterator[T]): CC[T]
  def toIterator[T](xs: CC[T]): Iterator[T]
  def map[T, U](xs: CC[T], f: T => U): CC[U] =
    fromIterator(toIterator(xs).map(f))
  def filter[T](xs: CC[T], p: T => Boolean): CC[T] =
    fromIterator(toIterator(xs).filter(p))
  def flatMap[T, U](xs: CC[T], f: T => TraversableOnce[U]): CC[U] =
    fromIterator(toIterator(xs).flatMap(f))
}

abstract class IterableCompanion[+CC[X] <: Iterable[X] with Collection[CC, X]]
extends CollectionCompanion[CC] with IterableImpls[CC] @uncheckedVariance {
  def toIterator[T](xs: CC[T] @uncheckedVariance) = xs.iterator
  implicit def transformOps[T](xs: CC[T] @uncheckedVariance): TransformOps[CC, T] = new TransformOps[CC, T](xs)
}

class TransformOps[+CC[X] <: Iterable[X] with Collection[CC, X], T] (val xs: CC[T]) extends AnyVal {
  def companion[T](xs: CC[T] @uncheckedVariance): IterableCompanion[CC] = xs.companion.asInstanceOf
  def map[U](f: T => U): CC[U] = companion(xs).map(xs, f)
  def filter(p: T => Boolean): CC[T] = companion(xs).filter(xs, p)
  def flatMap[U](f: T => TraversableOnce[U]): CC[U] = companion(xs).flatMap(xs, f)
}

object Iterable extends IterableCompanion[Iterable] {
  def fromIterator[T](it: Iterator[T]): Iterable[T] = ???
}
object Seq extends IterableCompanion[Seq] {
  def fromIterator[T](it: Iterator[T]): Seq[T] = ???
}

