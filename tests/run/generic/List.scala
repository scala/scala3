package generic

import Shapes._

/** enum List[T] {
 *    case Cons(x: T, xs: List[T])
 *    case Nil()
 *  }
 */
sealed trait List0[T] extends Enum
object List0 {
  abstract case class Cons[T](hd: T, tl: List0[T]) extends List0[T] {
    def ordinal = 0
  }
  object Cons {
    def apply[T](x: T, xs: List0[T]): List0[T] = new Cons(x, xs) {}
    implicit def ConsShape[T]: Cons[T] `shaped` Prod[T, List0[T]] =
      new (Cons[T] `shaped` Prod[T, List0[T]]) {
        def toShape(x: Cons[T]) = Prod(x.hd, x.tl)
        def fromShape(p: Prod[T, List0[T]]) = new Cons(p.fst, p.snd) {}
      }
  }

  abstract case class Nil[T]() extends List0[T] {
    def ordinal = 1
  }
  object Nil {
    def apply[T](): List0[T] = new Nil[T]() {}
    implicit def NilShape[T]: Nil[T] `shaped` Unit =
      new (Nil[T] `shaped` Unit) {
        def toShape(x: Nil[T]) = ()
        def fromShape(x: Unit) = new Nil[T]() {}
      }
  }

  implicit def List0Shape[T]: List0[T] `shaped` Sum[Cons[T], Nil[T]] =
    new (List0[T] `shaped` Sum[Cons[T], Nil[T]]) {
      def toShape(x: List0[T]) = x match {
        case x: Cons[T] => Fst(x)
        case x: Nil[T] => Snd(x)
      }
      def fromShape(x: Sum[Cons[T], Nil[T]]) = x match {
        case Fst(c) => c
        case Snd(n) => n
      }
    }
}

/** enum List[+T] {
 *    case Cons(x: T, xs: List[T])
 *    case Nil extends List[Nothing]
 *  }
 */
sealed trait List[+T] extends Enum
object List {
  abstract case class Cons[T](hd: T, tl: List[T]) extends List[T] {
    def ordinal = 0
  }
  object Cons {
    def apply[T](x: T, xs: List[T]): List[T] = new Cons(x, xs) {}
    type Shape[T] = Prod[T, List[T]]
    implicit def ConsShape[T]: Cons[T] `shaped` Shape[T] =
      new (Cons[T] `shaped` Shape[T]) {
        def toShape(x: Cons[T]) = Prod(x.hd, x.tl)
        def fromShape(p: Shape[T]) = new Cons(p.fst, p.snd) {}
      }
  }

  val Nil = new List[Nothing] {
    def ordinal = 1
    override def toString = "Nil"
  }

  implicit def NilSingleton: Singleton[Nil.type] = new Singleton[Nil.type](Nil)

  type Shape[T] = Sum[Cons[T], Nil.type]

  implicit def ListShape[T]: List[T] `unfolds` Shape[T] =
    new (List[T] `shaped` Shape[T]) {
      def toShape(x: List[T]) = x match {
        case x: Cons[T] => Fst(x)
        case Nil => Snd(Nil)
      }
      def fromShape(x: Shape[T]): List[T] = x match {
        case Fst(c) => c
        case Snd(n) => n
      }
    }
}