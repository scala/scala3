package mbsg

import dotty.{readonly, polyread, nonrep}

// Function
trait Function1[-T, +R] {
  def apply(t: T): R
}

trait Function2[-T1, -T2, +R] {
  def apply(t1: T1, t2: T2): R
}



// Tuple
case class Tuple2[+T1, +T2](__1: T1, __2: T2) {
  @readonly override def toString() = "(" + _1.toString + "," + _2.toString + ")"
}



// Builder
trait Builder[-T, +To] {

  def +=(e1: T): Unit
  def finalise: To
}

// Can build from implicit
trait CanBuildFrom[-From, -Elem, +To] {

  def apply(): Builder[Elem, To]
}

class ListBuilder[T] extends Builder[T, List[T]] {

  private var start: List[T] = Nil
  private var last0: List[T] = Nil
  private var len: Int = 0

  def += (x: T): Unit = {
    if (start.isEmpty) {
      last0 = new :: (x, Nil)
      start = last0
    } else {
      val last1 = last0.asInstanceOf[::[T]]
      last0 = new :: (x, Nil)
      last1.tail = last0
    }
    len += 1
  }

  def finalise: List[T] = start
}



// Numeric
trait Numeric[T] {
  def plus(x: T, y: T): T
  def zero: T
}



// Traversable
trait Traversable[+T] extends TraversableLike[T, Traversable[T]]

trait TraversableLike[+T, +Repr] {

  @readonly def foreach[U](f: Function1[T, U]): Unit

  @readonly def foldLeft[B](z: B)(op: Function2[B, T, B]): B

  @readonly def mapTo[U, To](f: Function1[T, U])(b: Builder[U, To]): To = {
    foreach(new Function1[T,Unit] { def apply(t: T): Unit = b += f(t) })
    b.finalise
  }

  @readonly def map[U, That](f: Function1[T, U])(implicit cbf: CanBuildFrom[Repr, U, That]): That = mapTo[U, That](f)(cbf())

  @readonly def sum[B >: T](implicit n : Numeric[B]): B = foldLeft(n.zero) {
    new Function2[B, T, B] { def apply(b: B, t: T): B = n.plus(b, t) }
  }
}


// Iterable
trait Iterable[+T] extends Traversable[T] with IterableLike[T, Iterable[T]] {
  @readonly def iterator: Iterator[T]
}

trait IterableLike[+T, +Repr] extends Traversable[T] {

  @readonly def iterator: Iterator[T]

  @readonly def zipTo[B, To](that: Iterable[B] @readonly)(b: Builder[Tuple2[T, B], To]): To = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += (new Tuple2(these.next, those.next))
    b.finalise
  }

  @readonly def zip[U, That](that: Iterable[U] @readonly)(implicit cbf: CanBuildFrom[Repr, Tuple2[T, U], That]): That = zipTo[U, That](that)(cbf())
}


// Iterator
trait Iterator[+T] {
  def hasNext(): Boolean
  def next(): T
}


trait LinearSeqOptimized[+A] extends Iterable[A] {

  @readonly def isEmpty: Boolean

  @readonly def head: A

  @polyread def tail: LinearSeqOptimized[A] @polyread

  @readonly override /*IterableLike*/
  def foreach[B](f: Function1[A, B]): Unit = {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  @readonly override /*TraversableLike*/
  def foldLeft[B](z: B)(f: Function2[B, A, B]): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }
}


// List
abstract class List[+T] extends Traversable[T] with TraversableLike[T, List[T]] with Iterable[T] with IterableLike[T, List[T]] with LinearSeqOptimized[T] {

  @readonly def iterator = new Iterator[T] {
    var current = List.this
    def hasNext = current != Nil
    def next() = {
      val t = current.head
      current = current.tail
      t
    }
  }

  @readonly def isEmpty: Boolean

  @readonly @inline override final
  def foreach[U](f: Function1[T, U]): Unit = {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  @readonly def head: T
  @polyread def tail: List[T] @polyread
  @readonly def size: Int

  def ::[S >: T](e1: S) : List[S] = new ::[S](e1, this)

  @readonly def reverse: List[T] = {
    val it = iterator
    var list: List[T] = Nil
    while (it.hasNext) list = it.next :: list
    list
  }
}

object List {
  implicit def canBuildFrom[A]: CanBuildFrom[List[_], A, List[A]] =
    new CanBuildFrom[List[_], A, List[A]] {
      def apply = new ListBuilder[A]
    }
}

case class ::[T](@nonrep head: T, var tail: List[T]) extends List[T] {
  @readonly def size = 1 + tail.size

  @readonly def isEmpty: Boolean = false

  @readonly override def toString = head.toString + " :: " + tail.toString
}

case object Nil extends List[Nothing] {
  @readonly def head = throw new NoSuchElementException("head of empty list")
  @polyread def tail = throw new NoSuchElementException("tail of empty list")

  @readonly def size = 0

  @readonly def isEmpty: Boolean = true

  @readonly override def toString = "Nil"
}


object Test {
  case class Box(var f: Int)
  val l = Box(1) :: Box(2) :: Box(3) :: Nil
  l.map{b => b.f = 5; b}
  val l2 = l: List[Box @readonly]
//  l2.map{b => b.f = 5; b}
}
