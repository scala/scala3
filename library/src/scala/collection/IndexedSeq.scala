/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.annotation.{nowarn, tailrec}
import scala.collection.Searching.{Found, InsertionPoint, SearchResult}
import scala.collection.Stepper.EfficientSplit
import scala.math.Ordering

/** Base trait for indexed sequences that have efficient `apply` and `length`. */
trait IndexedSeq[+A] extends Seq[A]
  with IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]]
  with IterableFactoryDefaults[A, IndexedSeq] {
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "IndexedSeq"

  override def iterableFactory: SeqFactory[IndexedSeq] = IndexedSeq
}

@SerialVersionUID(3L)
object IndexedSeq extends SeqFactory.Delegate[IndexedSeq](immutable.IndexedSeq)

/** Base trait for indexed Seq operations. */
transparent trait IndexedSeqOps[+A, +CC[_], +C] extends Any with SeqOps[A, CC, C] { self: IndexedSeqOps[A, CC, C]^ =>

  def iterator: Iterator[A]^{this} = view.iterator

  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntIndexedSeqStepper   (this.asInstanceOf[IndexedSeqOps[Int, AnyConstr, ?]],    0, length)
      case StepperShape.LongShape   => new LongIndexedSeqStepper  (this.asInstanceOf[IndexedSeqOps[Long, AnyConstr, ?]],   0, length)
      case StepperShape.DoubleShape => new DoubleIndexedSeqStepper(this.asInstanceOf[IndexedSeqOps[Double, AnyConstr, ?]], 0, length)
      case _                        => shape.parUnbox(new AnyIndexedSeqStepper[A](this, 0, length))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  override def reverseIterator: Iterator[A]^{this} = view.reverseIterator

  /* TODO 2.14+ uncomment and delete related code in IterableOnce
  @tailrec private def foldl[B](start: Int, end: Int, z: B, op: (B, A) => B): B =
    if (start == end) z
    else foldl(start + 1, end, op(z, apply(start)), op)
   */

  @tailrec private def foldr[B](start: Int, end: Int, z: B, op: (A, B) => B): B =
    if (start == end) z
    else foldr(start, end - 1, op(apply(end - 1), z), op)

  //override def foldLeft[B](z: B)(op: (B, A) => B): B = foldl(0, length, z, op)

  override def foldRight[B](z: B)(op: (A, B) => B): B = foldr(0, length, z, op)

  //override def reduceLeft[B >: A](op: (B, A) => B): B = if (length > 0) foldl(1, length, apply(0), op) else super.reduceLeft(op)

  //override def reduceRight[B >: A](op: (A, B) => B): B = if (length > 0) foldr(0, length - 1, apply(length - 1), op) else super.reduceRight(op)

  override def view: IndexedSeqView[A]^{this} = new IndexedSeqView.Id[A](this)

  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  override def view(from: Int, until: Int): IndexedSeqView[A]^{this} = view.slice(from, until)

  override protected def reversed: Iterable[A]^{this} = new IndexedSeqView.Reverse(this)

  // Override transformation operations to use more efficient views than the default ones
  override def prepended[B >: A](elem: B): CC[B]^{this} = iterableFactory.from(new IndexedSeqView.Prepended(elem, this))

  override def take(n: Int): C^{this} = fromSpecific(new IndexedSeqView.Take(this, n))

  override def takeRight(n: Int): C^{this} = fromSpecific(new IndexedSeqView.TakeRight(this, n))

  override def drop(n: Int): C^{this} = fromSpecific(new IndexedSeqView.Drop(this, n))

  override def dropRight(n: Int): C^{this} = fromSpecific(new IndexedSeqView.DropRight(this, n))

  override def map[B](f: A => B): CC[B]^{this, f} = iterableFactory.from(new IndexedSeqView.Map(this, f))

  override def reverse: C^{this} = fromSpecific(new IndexedSeqView.Reverse(this))

  override def slice(from: Int, until: Int): C^{this} = fromSpecific(new IndexedSeqView.Slice(this, from, until))

  override def sliding(size: Int, step: Int): Iterator[C^{this}]^{this} = {
    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")
    val it = new IndexedSeqSlidingIterator[A, CC, C](this, size, step)
    it.asInstanceOf[Iterator[Nothing]] // TODO: seems like CC cannot figure this out yet
  }

  override def head: A =
    if (!isEmpty) apply(0)
    else throw new NoSuchElementException(s"head of empty ${
      self match {
        case self: IndexedSeq[_] => self.collectionClassName
        case _ => toString
      }
    }")

  override def headOption: Option[A] = if (isEmpty) None else Some(head)

  override def last: A =
    if (!isEmpty) apply(length - 1)
    else throw new NoSuchElementException(s"last of empty ${
      self match {
        case self: IndexedSeq[_] => self.collectionClassName
        case _ => toString
      }
    }")

  // We already inherit an efficient `lastOption = if (isEmpty) None else Some(last)`

  override final def lengthCompare(len: Int): Int = Integer.compare(length, len)

  override def knownSize: Int = length

  override final def lengthCompare(that: Iterable[?]^): Int = {
    val res = that.sizeCompare(length)
    // can't just invert the result, because `-Int.MinValue == Int.MinValue`
    if (res == Int.MinValue) 1 else -res
  }

  override def search[B >: A](elem: B)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, 0, length)(using ord)

  override def search[B >: A](elem: B, from: Int, to: Int)(implicit ord: Ordering[B]): SearchResult =
    binarySearch(elem, from, to)(using ord)

  @tailrec
  private def binarySearch[B >: A](elem: B, from: Int, to: Int)
                                        (implicit ord: Ordering[B]): SearchResult = {
    if (from < 0) binarySearch(elem, 0, to)
    else if (to > length) binarySearch(elem, from, length)
    else if (to <= from) InsertionPoint(from)
    else {
      val idx = from + (to - from - 1) / 2
      math.signum(ord.compare(elem, apply(idx))) match {
        case -1 => binarySearch(elem, from, idx)(using ord)
        case  1 => binarySearch(elem, idx + 1, to)(using ord)
        case  _ => Found(idx)
      }
    }
  }
}

/** A fast sliding iterator for IndexedSeqs which uses the underlying `slice` operation. */
private final class IndexedSeqSlidingIterator[A, CC[_], C](s: IndexedSeqOps[A, CC, C]^, size: Int, step: Int)
  extends AbstractIterator[C^{s}] {
  // CC note: seems like the compiler cannot figure out that this class <: Iterator[C^{s}],
  // so we need a cast when upcasting is needed.

  private val len = s.length
  private var pos = 0
  private def chklen: Boolean = len == s.length || {
    throw new java.util.ConcurrentModificationException("collection size changed during iteration")
    false
  }

  def hasNext: Boolean = chklen && pos < len

  def next(): C^{s} = if (!chklen || !hasNext) Iterator.empty.next() else {
    val end = { val x = pos + size; if (x < 0 || x > len) len else x } // (pos.toLong + size).min(len).toInt
    val slice = s.slice(pos, end)
    pos =
      if (end >= len) len
      else { val x = pos + step; if (x < 0 || x > len) len else x } // (pos.toLong + step).min(len).toInt
    slice
  }
}
