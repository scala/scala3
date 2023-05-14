import scala.collection.mutable
import scala.annotation.tailrec

object TypeLevel {
  /** @param caseLabels The case and element labels of the described ADT as encoded strings.
  */
  class GenericClass(labelsStr: String) {
    import GenericClass.*

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

    val label: Array[Array[String]] =
      initLabels(0, 0, new mutable.ArrayBuffer[String], new mutable.ArrayBuffer[Array[String]])

    private final val elemSeparator = '\u0000'
    private final val caseSeparator = '\u0001'

    private def initLabels(start: Int, cur: Int,
                           elems: mutable.ArrayBuffer[String],
                           cases: mutable.ArrayBuffer[Array[String]]): Array[Array[String]] = {
      def addElem = elems += labelsStr.substring(start, cur)
      def addCase = cases += addElem.toArray
      if (cur == labelsStr.length)
        addCase.toArray
      else if (labelsStr(cur) == caseSeparator)
        initLabels(cur + 1, cur + 1, new mutable.ArrayBuffer, addCase)
      else if (labelsStr(cur) == elemSeparator)
        initLabels(cur + 1, cur + 1, addElem, cases)
      else
        initLabels(start, cur + 1, elems, cases)
    }
  }

  object GenericClass {
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

  /** A generic representation of a case in an ADT
  *  @param  deriving  The companion object of the ADT
  *  @param  ordinal   The ordinal value of the case in the list of the ADT's cases
  *  @param  elems     The elements of the case
  */
  class Mirror(val reflected: GenericClass, val ordinal: Int, val elems: Product) {

    /** The `n`'th element of this generic case */
    def apply(n: Int): Any = elems.productElement(n)

    /** The name of the constructor of the case reflected by this mirror */
    def caseLabel: String = reflected.label(ordinal)(0)

    /** The label of the `n`'th element of the case reflected by this mirror */
    def elementLabel(n: Int) = reflected.label(ordinal)(n + 1)
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
    def common: GenericClass
  }

  /** The shape of an ADT.
   *  This is eithe a product (`Case`) or a sum (`Cases`) of products.
   */
  enum Shape {

    /** A sum with alternative types `Alts` */
    case Cases[Alts <: Tuple]()

    /** A product type `T` with element types `Elems` */
    case Case[T, Elems <: Tuple]()
  }

  /** Every generic derivation starts with a type class instance of this type.
   *  It informs that type `T` has shape `S` and also implements runtime reflection on `T`.
   */
  abstract class Shaped[T, S <: Shape] extends Reflected[T]

  // substitute for erasedValue that allows precise matching
  final abstract class Type[-A, +B]
  type Subtype[t] = Type[_, t]
  type Supertype[t] = Type[t, _]
  type Exactly[t] = Type[t, t]
  erased def typeOf[T]: Type[T, T] = compiletime.erasedValue
}

// An algebraic datatype
enum Lst[+T] {
  case Cons[T](hd: T, tl: Lst[T]) extends Lst[T]
  case Nil
}

object Lst {
  // common compiler-generated infrastructure
  import TypeLevel.*

  type Shape[T] = Shape.Cases[(
    Shape.Case[Cons[T], (T, Lst[T])],
    Shape.Case[Nil.type, EmptyTuple]
  )]

  val genericClass = new GenericClass("Cons\u0000hd\u0000tl\u0001Nil")
  import genericClass.mirror

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
    def common = genericClass
  }

  // three clauses that could be generated from a `derives` clause
  implicit def LstShow[T: Show]: Show[Lst[T]] = Show.derived
}

// A simple product type
case class Pair[T](x: T, y: T) // derives Eq, Pickler, Show

object Pair {
  // common compiler-generated infrastructure
  import TypeLevel.*

  type Shape[T] = Shape.Case[Pair[T], (T, T)]

  val genericClass = new GenericClass("Pair\u0000x\u0000y")
  import genericClass.mirror

  implicit def pairShape[T]: Shaped[Pair[T], Shape[T]] = new {
    def reflect(xy: Pair[T]) =
      mirror(0, xy)
    def reify(c: Mirror): Pair[T] =
      Pair(c(0).asInstanceOf, c(1).asInstanceOf)
    def common = genericClass
  }
}

sealed trait Either[+L, +R] extends Product with Serializable // derives Eq, Pickler, Show
case class Left[L](x: L) extends Either[L, Nothing]
case class Right[R](x: R) extends Either[Nothing, R]

object Either {
  import TypeLevel.*

  type Shape[L, R] = Shape.Cases[(
    Shape.Case[Left[L], L *: EmptyTuple],
    Shape.Case[Right[R], R *: EmptyTuple]
  )]

  val genericClass = new GenericClass("Left\u0000x\u0001Right\u0000x")
  import genericClass.mirror

  implicit def eitherShape[L, R]: Shaped[Either[L, R], Shape[L, R]] = new {
    def reflect(e: Either[L, R]): Mirror = e match {
      case e: Left[L] => mirror(0, e)
      case e: Right[R] => mirror(1, e)
    }
    def reify(c: Mirror): Either[L, R] = c.ordinal match {
      case 0 => Left[L](c(0).asInstanceOf)
      case 1 => Right[R](c(0).asInstanceOf)
    }
    def common = genericClass
  }
  implicit def EitherShow[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

trait Show[T] {
  def show(x: T): String
}
object Show {
  import scala.compiletime.{erasedValue, error, summonInline}
  import TypeLevel.*

  inline def tryShow[T](x: T): String = summonInline[Show[T]].show(x)

  inline def showElems[Elems <: Tuple](elems: Mirror, n: Int): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        val formal = elems.elementLabel(n)
        val actual = tryShow[elem](elems(n).asInstanceOf)
        s"$formal = $actual" :: showElems[elems1](elems, n + 1)
      case _: EmptyTuple =>
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
        inline typeOf[alt] match {
          case _: Subtype[T] =>
            x match {
              case x: `alt` => showCase[T, elems](r, x)
              case _ => showCases[T, alts1](r, x)
            }
          case _ =>
            error("invalid call to showCases: one of Alts is not a subtype of T")
        }
      case _: EmptyTuple =>
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
  import TypeLevel.*

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
