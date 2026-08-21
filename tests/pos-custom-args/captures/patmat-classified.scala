import caps.*
trait Iterator[+A] extends IterableOnce[A]{ self: Iterator[A]^ =>
  def hasNext: Boolean
  def next(): A
}

/** Iterator can be used only once */
trait IterableOnce[+A] {
  this: IterableOnce[A]^ =>
  def iterator: Iterator[A]^{this}
}

/** Base trait for generic collections */
trait Iterable[+A] extends IterableOnce[A] {
  this: Iterable[A]^ =>
  type C[X] <: Iterable[X]^
  protected def coll: Iterable[A]^{this} = this
  def knownLength: Int = -1
}

/** Base trait for sequence collections */
trait Seq[+A] extends Iterable[A] {
  this: Seq[A]^ =>
  type C[X] <: Seq[X]^
  def apply(i: Int): A
  def length: Int
}

/** Base trait for collection builders */
trait Builder[-A, +To] extends Mutable {
  update def +=(x: A): this.type
  def result: To
}

abstract class ArrayBuffer[A] extends IterableOnce[A], Builder[A, ArrayBuffer[A]]:
  def ++[B >: A](xs: IterableOnce[B]^): ArrayBuffer[B]^ = xs match
    case xs: ArrayBuffer[B] @unchecked =>
      xs.asInstanceOf[ArrayBuffer[B]^]

trait FromIterable {
  type C[X] <: Iterable[X]^
  def fromIterable[B](it: Iterable[B]^): C[B]^{it, any}
}

/** Base trait for companion objects of collections */
trait IterableFactory extends FromIterable

trait SeqFactory extends IterableFactory {
  type C[X] <: Seq[X]^
  def fromIterable[B](it: Iterable[B]^{this, any}): C[B]
}
