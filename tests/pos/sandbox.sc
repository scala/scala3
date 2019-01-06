val xs = (1, 2)
def ys = ("true", true)
val zs = xs ++ ys

val x = zs.size
def toSeq(xs: NonEmptyTuple): Seq[Any] =
  for (i <- 0 until xs.size) yield xs(i)
val zs1 = toSeq(zs)
zs1.length

val hd *: tail = zs
val x1 = zs ++ zs.tail
val x2 = x1 ++ x1 ++ x1
type Reverse[X <: Tuple] <: Tuple = X match {
  case Unit => Unit
  case h *: t => Tuple.Concat[Reverse[t], h *: Unit]
}
def reverse[X <: Tuple](x: X): Reverse[X] = {
  x match {
    case () => x
    case x: (hd *: tl) => reverse(x.tail) ++ (x.head *: ())
  }
}.asInstanceOf

reverse(zs)

inline def greeting(name: String) = inline name match {
  case "Bob" => "Hello, Bob!"
  case "Anna" => "Hi, Anna!"
}
greeting("Anna")

object tc {
  import scala.reflect._
  import scala.compiletime._

  trait Ord[T] {
    def compare(x: T, y: T): Int
  }
  object Ord {
    inline def derived[T](implicit gen: Generic[T]): Ord[T] = new {
      def compare(x: T, y: T): Int = {
        val mx = gen.reflect(x)
        val my = gen.reflect(y)
        if (mx.ordinal != my.ordinal) my.ordinal - mx.ordinal
        else inline erasedValue[gen.Shape] match {
          case _: Shape.Cases[alts] =>
            ordCases[alts](mx, my, 0)
        }
      }
    }
    inline def ordCases[Alts](mx: Mirror, my: Mirror, n: Int): Int =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[_, elems] *: alts1) =>
          if (mx.ordinal == n) ordCase[elems](mx, my, 0)
          else ordCases[alts1](mx, my, n + 1)
        case _: Unit =>
          throw new MatchError(mx.ordinal)
      }
    inline def ordCase[Elems](mx: Mirror, my: Mirror, n: Int): Int =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          implicit match {
            case cmp: Ord[`elem`] =>
              val r = cmp.compare(mx(n).asInstanceOf, my(n).asInstanceOf)
              if (r != 0) r
              else ordCase[elems1](mx, my, n + 1)
          }
        case _: Unit =>
          0
      }
    implicit object intOrd extends Ord[Int] {
      def compare(x: Int, y: Int): Int =
        if (x < y) -1 else if (x > y) 1 else 0
    }
    implicit object stringOrd extends Ord[String] {
      def compare(x: String, y: String): Int =
        x.compareTo(y)
    }
    implicit object booleanOrd extends Ord[Boolean] {
      def compare(x: Boolean, y: Boolean): Int =
        x.compareTo(y)
    }
    implicit object nothingOrd extends Ord[Nothing] {
      def compare(x: Nothing, y: Nothing): Int = {
        assert(false)
        0
      }
    }
  }
}

import tc._
enum Tree[T] derives Ord {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}
def (x: T) <= [T: Ord] (y: T): Boolean =
  implicitly[Ord[T]].compare(x, y) <= 0
import Tree._
val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
t <= t
val l = Leaf(true)
l <= Leaf(false)

implicit def ordList[T: Ord]: Ord[List[T]] = Ord.derived
List(1, 2, 3) <= List(1, 2, 0)
List(1, 2, 3) <= List(1, 2)
List(1, 2) <= List(1, 2, 3)
