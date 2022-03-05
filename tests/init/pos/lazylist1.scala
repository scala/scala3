import scala.language.implicitConversions

class LazyList[A]

object LazyList {
  inline implicit def toDeferred[A](l: LazyList[A]): Deferred[A] =
    new Deferred(l)

  final class Deferred[A](l: => LazyList[A]) {
    def #:: [B >: A](elem: => B): LazyList[B] = ???
  }
}

import LazyList.*

final class Test {
  lazy val a: LazyList[Int] = 5 #:: b
  lazy val b: LazyList[Int] = 10 #:: a

  val x: LazyList[Int] = 5 #:: y
  val y: LazyList[Int] = 10 #:: x
}