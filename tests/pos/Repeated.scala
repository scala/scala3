package scala.collection
package immutable

import scala.collection.Stepper.EfficientSplit
import scala.collection.immutable.Range
import scala.collection.mutable.Buffer
import scala.collection.IterableOps.SizeCompareOps
import scala.collection.Searching.SearchResult
import scala.math.Ordering
import scala.reflect.ClassTag
import scala.util.Sorting
import annotation.targetName

opaque type Repeated[+A] = Seq[A]
object Repeated:

  extension [A](xs: Repeated[A])
    def length: Int = xs.length
    def apply(n: Int) = xs.apply(n)
    def iterator: Iterator[A] = xs.iterator

    def ++[B >: A](suffix: IterableOnce[B]): Repeated[B] = xs ++ suffix
    def :+ [B >: A](x: B): Repeated[B] = xs :+ x
    def :++ [B >: A](suffix: IterableOnce[B]): Repeated[B] = xs :++ suffix

    def addString(b: mutable.StringBuilder): mutable.StringBuilder = xs.addString(b)
    def addString(b: mutable.StringBuilder, sep: String): mutable.StringBuilder = xs.addString(b, sep)
    def addString(b: mutable.StringBuilder, start: String, sep: String, end: String): mutable.StringBuilder = xs.addString(b, start, sep, end)
    def appended[B >: A](x: B): Repeated[B] = xs.appended(x)
    def appendedAll[B >: A](suffix: IterableOnce[B]): Repeated[B] = xs.appendedAll(suffix)
    def collect[B](pf: PartialFunction[A, B]): Repeated[B] = xs.collect(pf)
    def collectFirst[B](f: PartialFunction[A, B]): Option[B] = xs.collectFirst(f)
    def combinations(n: Int): Iterator[Repeated[A]] = xs.combinations(n)
    def concat[B >: A](suffix: IterableOnce[B]): Repeated[B] = xs.concat(suffix)
    def contains(elem: A): Boolean = xs.contains(elem)
    def containsSlice[B](that: Seq[B]): Boolean = xs.containsSlice(that)
    def copyToArray[B >: A](ys: Array[B]): Int = xs.copyToArray(ys)
    def copyToArray[B >: A](ys: Array[B], start: Int): Int = xs.copyToArray(ys, start)
    def copyToArray[B >: A](ys: Array[B], start: Int, len: Int): Int = xs.copyToArray(ys, start, len)
    def corresponds[B](that: IterableOnce[B])(p: (A, B) => Boolean): Boolean = xs.corresponds(that)(p)
    def count(p: A => Boolean): Int = xs.count(p)
    def diff[B >: A](that: Seq[B]): Repeated[A] = xs.diff(that)
    def distinct: Repeated[A] = xs.distinct
    def distinctBy[B](f: A => B): Repeated[A] = xs.distinctBy(f)
    def drop(n: Int): Repeated[A] = xs.drop(n)
    def dropRight(n: Int): Repeated[A] = xs.dropRight(n)
    def dropWhile(p: A => Boolean): Repeated[A] = xs.dropWhile(p)
    def empty: Repeated[A] = xs.empty
    def endsWith[B >: A](that: Iterable[B]): Boolean = xs.endsWith(that)
    def exists(p: A => Boolean): Boolean = xs.exists(p)
    def filter(p: A => Boolean): Repeated[A] = xs.filter(p)
    def filterNot(p: A => Boolean): Repeated[A] = xs.filterNot(p)
    def find(p: A => Boolean): Option[A] = xs.find(p)
    def findLast(p: A => Boolean): Option[A] = xs.findLast(p)
    def flatMap[B](f: A => IterableOnce[B]): Repeated[B] = xs.flatMap(f)
    def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = xs.fold(z)(op)
    def foldLeft[B](z: B)(op: (B, A) => B): B = xs.foldLeft(z)(op)
    def foldRight[B](z: B)(op: (A, B) => B): B = xs.foldRight(z)(op)
    def forall(p: A => Boolean): Boolean = xs.forall(p)
    def foreach[U](f: A => U): Unit = xs.foreach(f)
    def groupBy[K](f: A => K): immutable.Map[K, Repeated[A]] = xs.groupBy(f)
    def groupMap[K, B](key: A => K)(f: A => B): immutable.Map[K, Repeated[B]] = xs.groupMap(key)(f)
    def groupMapReduce[K, B](key: (A) => K)(f: (A) => B)(reduce: (B, B) => B): immutable.Map[K, B] = xs.groupMapReduce(key)(f)(reduce)
    def grouped(size: Int): Iterator[Repeated[A]] = xs.grouped(size)
    def head: A = xs.head
    def headOption: Option[A] = xs.headOption
    def indexOf(elem: A, from: Int = 0): Int = xs.indexOf(elem, from)
    def indexOfSlice[B >: A](that: Seq[B]): Int = xs.indexOfSlice(that)
    def indexOfSlice[B >: A](that: Seq[B], from: Int): Int = xs.indexOfSlice(that, from)
    def indexWhere(p: A => Boolean, from: Int = 0): Int = xs.indexWhere(p, from)
    def indices: Range = xs.indices
    def init: Repeated[A] = xs.init
    def inits: Iterator[Repeated[A]] = xs.inits
    def intersect[B >: A](that: Seq[B]): Repeated[A] = xs.intersect(that)
    def isEmpty: Boolean = xs.isEmpty
    def isTraversableAgain: Boolean = xs.isTraversableAgain
    def knownSize: Int = xs.length
    def last: A = xs.last
    def lastIndexOf(elem: A, end: Int = xs.length - 1): Int = xs.lastIndexOf(elem, end)
    def lastIndexOfSlice[B >: A](that: Seq[B]): Int = xs.lastIndexOfSlice(that)
    def lastIndexOfSlice[B >: A](that: Seq[B], end: Int): Int = xs.lastIndexOfSlice(that, end)
    def lastIndexWhere(p: A => Boolean, end: Int = xs.length - 1): Int = xs.lastIndexWhere(p, end)
    def lastOption: Option[A] = xs.lastOption
    def lazyZip[B](that: Iterable[B]): LazyZip2[A, B, Repeated[A]] = xs.lazyZip[B](that).asInstanceOf[LazyZip2[A, B, Repeated[A]]]
    def lengthCompare(len: Int): Int = xs.lengthCompare(len)
    def lengthIs: SizeCompareOps = xs.lengthIs
    def map[B](f: A => B): Repeated[B] = xs.map(f)
    def max[B >: A](using math.Ordering[B]): A = xs.max[B]
    def maxBy[B](f: A => B)(using math.Ordering[B]): A = xs.maxBy(f)
    def maxByOption[B](f: A => B)(using math.Ordering[B]): Option[A] = xs.maxByOption(f)
    def maxOption[B >: A](using math.Ordering[B]): Option[B] = xs.maxOption[B]
    def min[B >: A](using math.Ordering[B]): A = xs.min[B]
    def minBy[B](f: A => B)(using math.Ordering[B]): A = xs.minBy(f)
    def minByOption[B](f: A => B)(using math.Ordering[B]): Option[A] = xs.minByOption(f)
    def minOption[B >: A](using math.Ordering[B]): Option[B] = xs.minOption[B]
    def mkString: String = xs.mkString
    def mkString(sep: String): String = xs.mkString(sep)
    def mkString(start: String, sep: String, end: String): String = xs.mkString(start, sep, end)
    def nonEmpty: Boolean = xs.nonEmpty
    def padTo[B >: A](len: Int, elem: B): Repeated[B] = xs.padTo(len, elem)
    def partition(p: A => Boolean): (Repeated[A], Repeated[A]) = xs.partition(p)
    def partitionMap[A1, A2](f: A => Either[A1, A2]): (Repeated[A1], Repeated[A2]) = xs.partitionMap(f)
    def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): Repeated[B] = xs.patch(from, other, replaced)
    def permutations: Iterator[Repeated[A]] = xs.permutations
    def prepended[B >: A](x: B): Repeated[B] = xs.prepended(x)
    def prependedAll[B >: A](prefix: IterableOnce[B]): Repeated[B] = xs.prependedAll(prefix)
    def product[B >: A](using math.Numeric[B]): B = xs.product[B]
    def reduce[B >: A](op: (B, B) => B): B = xs.reduce(op)
    def reduceLeft[B >: A](op: (B, A) => B): B = xs.reduceLeft(op)
    def reduceRight[B >: A](op: (A, B) => B): B = xs.reduceRight(op)
    def reverse: Repeated[A] = xs.reverse
    def reverseIterator: Iterator[A] = xs.reverseIterator
    def sameElements[B >: A](that: IterableOnce[B]): Boolean = xs.sameElements(that)
    def scan[B >: A](z: B)(op: (B, B) => B): Repeated[B] = xs.scan(z)(op)
    def scanLeft[B](z: B)(op: (B, A) => B): Repeated[B] = xs.scanLeft(z)(op)
    def scanRight[B](z: B)(op: (A, B) => B): Repeated[B] = xs.scanRight(z)(op)
    def search[B >: A](elem: B)(using Ordering[B]): SearchResult = xs.search(elem)
    def search[B >: A](elem: B, from: Int, to: Int)(using Ordering[B]): SearchResult = xs.search(elem, from, to)
    def segmentLength(p: (A) => Boolean, from: Int): Int = xs.segmentLength(p, from)
    def segmentLength(p: (A) => Boolean): Int = xs.segmentLength(p)
    def size: Int = xs.size
    def sizeCompare(that: Iterable[_]): Int = xs.sizeCompare(that)
    def sizeCompare(otherSize: Int): Int = xs.sizeCompare(otherSize)
    def sizeIs: SizeCompareOps = xs.sizeIs
    def slice(from: Int, until: Int): Repeated[A] = xs.slice(from, until)
    def sliding(size: Int, step: Int = 1): Iterator[Repeated[A]] = xs.sliding(size, step)
    def sortBy[B](f: A => B)(implicit ord: Ordering[B]): Repeated[A] = xs.sortBy(f)
    def sortWith(lt: (A, A) => Boolean): Repeated[A] = xs.sortWith(lt)
    def sorted[B >: A](implicit ord: Ordering[B]): Repeated[A] = xs.sorted[B]
    def span(p: A => Boolean): (Repeated[A], Repeated[A]) = xs.span(p)
    def splitAt(n: Int): (Repeated[A], Repeated[A]) = xs.splitAt(n)
    def startsWith[B >: A](that: IterableOnce[B], offset: Int): Boolean = xs.startsWith(that, offset)
    def stepper[S <: Stepper[_]](using StepperShape[A, S]): S = xs.stepper[S]
    def sum[B >: A](using math.Numeric[B]): B = xs.sum[B]
    def tail: Repeated[A] = xs.tail
    def tails: Iterator[Repeated[A]] = xs.tails
    def take(n: Int): Repeated[A] = xs.take(n)
    def takeRight(n: Int): Repeated[A] = xs.takeRight(n)
    def takeWhile(p: A => Boolean): Repeated[A] = xs.takeWhile(p)
    def tapEach[U](f: (A) => U): Repeated[A] = xs.tapEach(f)
    def to[C1](factory: Factory[A, C1]): C1 = xs.to(factory)
    def toArray[B >: A: ClassTag]: Array[B] = xs.toArray[B]
    def toBuffer[B >: A]: Buffer[B] = xs.toBuffer[B]
    def toIndexedSeq: immutable.IndexedSeq[A] = xs.toIndexedSeq
    def toIterable: Iterable[A] = xs.toIterable
    def toList: List[A] = xs.toList
    def toSeq: Seq[A] = xs.toSeq
    def toSet: Set[A] = xs.toSet
    def toVector: Vector[A] = xs.toVector
    def updated[B >: A](index: Int, elem: B): Repeated[B] = xs.updated(index, elem)
    def view: SeqView[A] = xs.view
    def withFilter(p: A => Boolean): WithFilter[A, Repeated] = xs.withFilter(p)
    def zip[B](that: IterableOnce[B]): Repeated[(A, B)] = xs.zip(that)
    def zipAll[A1 >: A, B](that: Iterable[B], thisElem: A1, thatElem: B): Repeated[(A1, B)] = xs.zipAll(that, thisElem, thatElem)
    def zipWithIndex: Repeated[(A, Int)] = xs.zipWithIndex

  extension [A](xs: Repeated[IterableOnce[A]])
    def flatten: Repeated[A] = xs.flatten

  extension [A](prefix: IterableOnce[A])
    def ++: (xs: Repeated[A]): Repeated[A] = xs.prependedAll(prefix)

  extension [A](x: A)
    def +: (xs: Repeated[A]): Repeated[A] = xs.prepended(x)

  extension [A1, A2](xs: Repeated[(A1, A2)])
    def unzip: (Repeated[A1], Repeated[A2]) = xs.unzip
    def toMap: Map[A1, A2] = xs.toMap

  extension [A1, A2, A3](xs: Repeated[(A1, A2, A3)])
    def unzip3: (Repeated[A1], Repeated[A2], Repeated[A3]) = xs.unzip3

  extension [A](xs: Repeated[Repeated[A]])
    def transpose: Repeated[Repeated[A]] = xs.transpose

  implicit def repeatedToSeq[A](xs: Repeated[A]): Seq[A] = xs.toSeq