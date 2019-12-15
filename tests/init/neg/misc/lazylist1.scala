class LazyList[A]

object LazyList {
  inline implicit def toDeferred[A](l: LazyList[A]): Deferred[A] =
    new Deferred(l)

  final class Deferred[A](_l: => LazyList[A]) {
    def #:: [B >: A](elem: => B): LazyList[B] = ???
  }

  inline implicit def toDeferred2[A](l: LazyList[A]): Deferred2[A] =
    new Deferred2(l)

  final class Deferred2[A](l: => LazyList[A]) {
    def *:: [B >: A](elem: => B): LazyList[B] = ???
  }
}

import LazyList._

final class Test {
  val a: LazyList[Int] = 5 #:: b
  val b: LazyList[Int] = 10 #:: a

  val x: LazyList[Int] = 5 *:: y
  val y: LazyList[Int] = 10 *:: x  // error
}