object Deriving {
  import scala.compiletime._

  sealed trait Shape

  class HasSumShape[T, S <: Tuple]

  abstract class HasProductShape[T, Xs <: Tuple] {
    def toProduct(x: T): Xs
    def fromProduct(x: Xs): T
  }

  enum Lst[+T] {
    case Cons(hd: T, tl: Lst[T])
    case Nil
  }

  object Lst {
    implicit def lstShape[T]: HasSumShape[Lst[T], (Cons[T], Nil.type)] = new HasSumShape

    implicit def consShape[T]: HasProductShape[Lst.Cons[T], (T, Lst[T])] = new {
      def toProduct(xs: Lst.Cons[T]) = (xs.hd, xs.tl)
      def fromProduct(xs: (T, Lst[T])): Lst.Cons[T] = Lst.Cons(xs(0), xs(1)).asInstanceOf
    }

    implicit def nilShape[T]: HasProductShape[Lst.Nil.type, Unit] = new {
      def toProduct(xs: Lst.Nil.type) = ()
      def fromProduct(xs: Unit) = Lst.Nil
    }

    implicit def LstEq[T: Eq]: Eq[Lst[T]] = Eq.derivedForSum
    implicit def ConsEq[T: Eq]: Eq[Cons[T]] = Eq.derivedForProduct
    implicit def NilEq[T]: Eq[Nil.type] = Eq.derivedForProduct
  }

  trait Eq[T] {
    def equals(x: T, y: T): Boolean
  }

  object Eq {
    inline def tryEq[T](x: T, y: T) = implied match {
      case eq: Eq[T] => eq.equals(x, y)
    }

    inline def deriveForSum[Alts <: Tuple](x: Any, y: Any): Boolean = inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        x match {
          case x: `alt` =>
            y match {
              case y: `alt` => tryEq[alt](x, y)
              case _ => false
            }
          case _ => deriveForSum[alts1](x, y)
        }
      case _: Unit =>
        false
    }

    inline def deriveForProduct[Elems <: Tuple](xs: Elems, ys: Elems): Boolean = inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        val xs1 = xs.asInstanceOf[elem *: elems1]
        val ys1 = ys.asInstanceOf[elem *: elems1]
        tryEq[elem](xs1.head, ys1.head) &&
        deriveForProduct[elems1](xs1.tail, ys1.tail)
      case _: Unit =>
        true
    }

    inline def derivedForSum[T, Alts <: Tuple](implicit ev: HasSumShape[T, Alts]): Eq[T] = new {
      def equals(x: T, y: T): Boolean = deriveForSum[Alts](x, y)
    }

    inline def derivedForProduct[T, Elems <: Tuple](implicit ev: HasProductShape[T, Elems]): Eq[T] = new {
      def equals(x: T, y: T): Boolean = deriveForProduct[Elems](ev.toProduct(x), ev.toProduct(y))
    }

    implicit object eqInt extends Eq[Int] {
      def equals(x: Int, y: Int) = x == y
    }
  }
}

object Test extends App {
  import Deriving._
  val eq = implicitly[Eq[Lst[Int]]]
  val xs = Lst.Cons(1, Lst.Cons(2, Lst.Cons(3, Lst.Nil)))
  val ys = Lst.Cons(1, Lst.Cons(2, Lst.Nil))
  assert(eq.equals(xs, xs))
  assert(!eq.equals(xs, ys))
  assert(!eq.equals(ys, xs))
  assert(eq.equals(ys, ys))

  val eq2 = implicitly[Eq[Lst[Lst[Int]]]]
  val xss = Lst.Cons(xs, Lst.Cons(ys, Lst.Nil))
  val yss = Lst.Cons(xs, Lst.Nil)
  assert(eq2.equals(xss, xss))
  assert(!eq2.equals(xss, yss))
  assert(!eq2.equals(yss, xss))
  assert(eq2.equals(yss, yss))
}