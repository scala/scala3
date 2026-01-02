import scala.annotation.unchecked.uncheckedVariance

private final class ConcatIterator[+A](val from: Iterator[A @uncheckedVariance]^) {
  self: ConcatIterator[A]^ =>
  def hasNext: Boolean = ???
  def next(): A = ???

  def concat[B >: A](that: => IterableOnce[B]^): ConcatIterator[B]^{this, that} = {
    val c = new ConcatIteratorCell[B](that, null).asInstanceOf[ConcatIteratorCell[A]]
      // crashes without special exemption for isRefining in CaptureSet.ProperVar$includeElem.
    this
  }
}

private final class ConcatIteratorCell[A](head: => IterableOnce[A]^, var tail: ConcatIteratorCell[A]^)
extends caps.Stateful:
  update def headIterator: Iterator[A]^{this} = head.iterator


