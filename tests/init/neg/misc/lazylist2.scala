import scala.language.implicitConversions

trait LazyList[+A] {
  def isEmpty: Boolean = true
  def head: A
  def tail: LazyList[A]
}

object LazyList {
  inline implicit def toHelper[A](l: => LazyList[A]): Helper[A] = new Helper(l)
  final class Helper[A](_l: => LazyList[A]) {
    def #:: [B >: A](elem: => B): LazyList[B] = new LazyList[B] {
      override def isEmpty: Boolean = false
      override def head: B = elem
      override def tail: LazyList[B] = _l
    }
  }

  implicit final class b[A](l: => LazyList[A]) {
    def *:: [B >: A](elem: => B): LazyList[B] = new LazyList[B] {
      override def isEmpty: Boolean = false
      override def head: B = elem
      override def tail: LazyList[B] = l
    }
  }
}

import LazyList._

final class Test1 {
  val a: LazyList[Int] = 5 #:: b
  val b: LazyList[Int] = 10 #:: a

  a.head // ok
  b.head // ok

  val x: LazyList[Int] = 5 *:: y
  val y: LazyList[Int] = 10 *:: x   // error
}

final class Test2 {
  val a: LazyList[Int] = 5 #:: b
  val b: LazyList[Int] = 10 #:: 30 #:: c
  val c: LazyList[Int] = 20 #:: a
}

final class Test3 {
  val a: LazyList[Int] = n #:: a
  a.head
  val n: Int = 20    // error
}