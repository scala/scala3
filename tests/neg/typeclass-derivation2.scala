import scala.collection.mutable
import scala.annotation.tailrec

trait Deriving {
  import Deriving._

  /** A mirror of case with ordinal number `ordinal` and elements as given by `Product` */
  def mirror(ordinal: Int, product: Product): Mirror =
    new Mirror(this, ordinal, product)

  /** A mirror with elements given as an array */
  def mirror(ordinal: Int, elems: Array[AnyRef]): Mirror =
    mirror(ordinal, new ArrayProduct(elems))

  /** A mirror with an initial empty array of `numElems` elements, to be filled in. */
  def mirror(ordinal: Int, numElems: Int): Mirror =
    mirror(ordinal, new Array[AnyRef](numElems))

  /** A mirror of a case with no elements */
  def mirror(ordinal: Int): Mirror =
    mirror(ordinal, EmptyProduct)

  /** The case and element labels of the described ADT as encoded strings. */
  protected def caseLabels: Array[String]

  private final val separator = '\000'

  private def label(ordinal: Int, idx: Int): String = {
    val labels = caseLabels(ordinal)
    @tailrec def separatorPos(from: Int): Int =
      if (from == labels.length || labels(from) == separator) from
      else separatorPos(from + 1)
    @tailrec def findLabel(count: Int, idx: Int): String =
      if (idx == labels.length) ""
      else if (count == 0) labels.substring(idx, separatorPos(idx))
      else findLabel(if (labels(idx) == separator) count - 1 else count, idx + 1)
    findLabel(idx, 0)
  }
}

// Generic deriving infrastructure
object Deriving {

  /** A generic representation of a case in an ADT
   *  @param  deriving  The companion object of the ADT
   *  @param  ordinal   The ordinal value of the case in the list of the ADT's cases
   *  @param  elems     The elements of the case
   */
  class Mirror(val deriving: Deriving, val ordinal: Int, val elems: Product) {

    /** The `n`'th element of this generic case */
    def apply(n: Int): Any = elems.productElement(n)

    /** The name of the constructor of the case reflected by this mirror */
    def caseLabel: String = deriving.label(ordinal, 0)

    /** The label of the `n`'th element of the case reflected by this mirror */
    def elementLabel(n: Int) = deriving.label(ordinal, n + 1)
  }

  /** A class for mapping between an ADT value and
   *  the case mirror that represents the value.
   */
  abstract class Reflected[T] {

    /** The case mirror corresponding to ADT instance `x` */
    def reflect(x: T): Mirror

    /** The ADT instance corresponding to given `mirror` */
    def reify(mirror: Mirror): T

    /** The companion object of the ADT */
    def deriving: Deriving
  }

  /** The shape of an ADT.
   *  This is eithe a product (`Case`) or a sum (`Cases`) of products.
   */
  enum Shape {

    /** A sum with alternative types `Alts` */
    case Cases[Alts <: Tuple]

    /** A product type `T` with element types `Elems` */
    case Case[T, Elems <: Tuple]
  }

  /** Every generic derivation starts with a typeclass instance of this type.
   *  It informs that type `T` has shape `S` and also implements runtime reflection on `T`.
   */
  abstract class Shaped[T, S <: Shape] extends Reflected[T]

  /** Helper class to turn arrays into products */
  private class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  /** Helper object */
  private object EmptyProduct extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = throw new IndexOutOfBoundsException
    def productArity = 0
  }
}

// An algebraic datatype
enum Lst[+T] {
  case Cons(hd: T, tl: Lst[T])
  case Nil
}

object Lst extends Deriving {
  // common compiler-generated infrastructure
  import Deriving._

  type Shape[T] = Shape.Cases[(
    Shape.Case[Cons[T], (T, Lst[T])],
    Shape.Case[Nil.type, Unit]
  )]

  val NilMirror = mirror(1)

  implicit def lstShape[T]: Shaped[Lst[T], Shape[T]] = new {
    def reflect(xs: Lst[T]): Mirror = xs match {
      case xs: Cons[T] => mirror(0, xs)
      case Nil => NilMirror
     }
    def reify(c: Mirror): Lst[T] = c.ordinal match {
      case 0 => Cons[T](c(0).asInstanceOf, c(1).asInstanceOf)
      case 1 => Nil
    }
    def deriving = Lst
  }

  protected val caseLabels = Array("Cons\000hd\000tl", "Nil")

  // three clauses that could be generated from a `derives` clause
  implicit def LstShow[T: Show]: Show[Lst[T]] = Show.derived
}

// A simple product type
case class Pair[T](x: T, y: T)

object Pair extends Deriving {
  // common compiler-generated infrastructure
  import Deriving._

  type Shape[T] = Shape.Case[Pair[T], (T, T)]

  implicit def pairShape[T]: Shaped[Pair[T], Shape[T]] = new {
    def reflect(xy: Pair[T]) =
      mirror(0, xy)
    def reify(c: Mirror): Pair[T] =
      Pair(c(0).asInstanceOf, c(1).asInstanceOf)
    def deriving = Pair
  }

  protected val caseLabels = Array("Pair\000x\000y")
}

sealed trait Either[+L, +R] extends Product
case class Left[L](x: L) extends Either[L, Nothing]
case class Right[R](x: R) extends Either[Nothing, R]

object Either extends Deriving {
  import Deriving._

  type Shape[L, R] = Shape.Cases[(
    Shape.Case[Left[L], L *: Unit],
    Shape.Case[Right[R], R *: Unit]
  )]

  implicit def eitherShape[L, R]: Shaped[Either[L, R], Shape[L, R]] = new {
    def reflect(e: Either[L, R]): Mirror = e match {
      case e: Left[L] => mirror(0, e)
      case e: Right[R] => mirror(1, e)
    }
    def reify(c: Mirror): Either[L, R] = c.ordinal match {
      case 0 => Left[L](c(0).asInstanceOf)
      case 1 => Right[R](c(0).asInstanceOf)
    }
    def deriving = Either
  }

  protected val caseLabels = Array("Left\000x", "Right\000x")

  implicit def EitherShow[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

trait Show[T] {
  def show(x: T): String
}
object Show {
  import scala.typelevel._
  import Deriving._

  inline def tryShow[T](x: T): String = implicit match {
    case s: Show[T] => s.show(x)
  }

  inline def showElems[Elems <: Tuple](elems: Mirror, n: Int): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        val formal = elems.elementLabel(n)
        val actual = tryShow[elem](elems(n).asInstanceOf)
        s"$formal = $actual" :: showElems[elems1](elems, n + 1)
      case _: Unit =>
        Nil
    }

  inline def showCase[T, Elems <: Tuple](r: Reflected[T], x: T): String = {
    val mirror = r.reflect(x)
    val args = showElems[Elems](mirror, 0).mkString(", ")
    s"${mirror.caseLabel}($args)"
  }

  inline def showCases[T, Alts <: Tuple](r: Reflected[T], x: T): String =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[alt, elems] *: alts1) =>
        x match {
          case x: `alt` => showCase[T, elems](r, x)
          case _ => showCases[T, alts1](r, x)
        }
      case _: Unit =>
        throw new MatchError(x)
    }

  inline def derived[T, S <: Shape](implicit ev: Shaped[T, S]): Show[T] = new {
    def show(x: T): String = inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        showCases[T, alts](ev, x)
      case _: Shape.Case[_, elems] =>
        showCase[T, elems](ev, x)
    }
  }

  implicit object IntShow extends Show[Int] {
    def show(x: Int): String = x.toString
  }
}

// Tests
object Test extends App {
  import Deriving._

  def showPrintln[T: Show](x: T): Unit =
    println(implicitly[Show[T]].show(x))

  val zs = Lst.Cons(Left(1), Lst.Cons(Right(Pair(2, 3)), Lst.Nil))
  showPrintln(zs)  // error
/* This should print as follows:
-- Error: typeclass-derivation2.scala:254:17 -----------------------------------
254 |  showPrintln(zs)
    |                 ^
    |no implicit argument of type Show[Lst[Either[Int, Pair[Int]]]] was found for parameter evidence$5 of method showPrintln in object Test.
    |I found:
    |
    |    Lst.LstShow[T](
    |      Either.EitherShow[Int, Pair[Int]](Show.IntShow,
    |        /* missing */implicitly[Show[Pair[Int]]]
    |      )
    |    )
    |
    |But no implicit values were found that match type Show[Pair[Int]].
*/
}