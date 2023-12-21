import scala.language.implicitConversions

trait LazyList[+A] {
  def isEmpty: Boolean = true
  def head: A
  def tail: LazyList[A]
}

object LazyList {
  implicit def toHelper[A](l: => LazyList[A]): Helper[A] = new Helper(l)
  final class Helper[A](l: => LazyList[A]) {
    def #:: [B >: A](elem: => B): LazyList[B] = new LazyList[B] {
      override def isEmpty: Boolean = false
      override def head: B = elem
      override def tail: LazyList[B] = l
    }
  }
}

import LazyList.*

final class Test1 {
  lazy val a: LazyList[Int] = 5 #:: b
  lazy val b: LazyList[Int] = 10 #:: a

  a.head // ok
  b.head // ok

  val x: LazyList[Int] = 5 #:: y   // warn
  val y: LazyList[Int] = 10 #:: x
}

final class Test2 {
  lazy val a: LazyList[Int] = 5 #:: b
  lazy val b: LazyList[Int] = 10 #:: 30 #:: c
  lazy val c: LazyList[Int] = 20 #:: a
}

final class Test3 {
  val a: LazyList[Int] = n #:: (a: @unchecked)    // warn
  a.head
  val n: Int = 20
}