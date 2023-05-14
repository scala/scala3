import scala.collection.mutable
import scala.annotation.tailrec

// A type class derivation encoding using Shape/Shaped scheme, now superseded by
// typeclass-derivation2a
object TypeLevel {
  /** @param caseLabels The case and element labels of the described ADT as encoded strings.
  */
  class ReflectedClass(labelsStr: String) {
    import ReflectedClass.*

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

    private final val elemSeparator = '\000'
    private final val caseSeparator = '\001'

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

  object ReflectedClass {
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
  class Mirror(val reflected: ReflectedClass, val ordinal: Int, val elems: Product) {

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
    def common: ReflectedClass
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
enum Lst[+T] { // derives Eq, Pickler, Show
  case Cons[T](hd: T, tl: Lst[T]) extends Lst[T]
  case Nil
}

object Lst {
  // common compiler-generated infrastructure
  import TypeLevel.*

  type Shape[T] = Shape.Cases[(
    Shape.Case[Cons[T], (T, Lst[T])],
    Shape.Case[Nil.type, Unit]
  )]

  val reflectedClass = new ReflectedClass("Cons\000hd\000tl\001Nil")
  import reflectedClass.mirror

  val NilMirror = mirror(1)

  implicit def derived$Shaped[T]: Shaped[Lst[T], Shape[T]] = new {
    def reflect(xs: Lst[T]): Mirror = xs match {
      case xs: Cons[T] => mirror(0, xs)
      case Nil => mirror(1)
     }
    def reify(c: Mirror): Lst[T] = c.ordinal match {
      case 0 => Cons[T](c(0).asInstanceOf, c(1).asInstanceOf)
      case 1 => Nil
    }
    def common = reflectedClass
  }

  // three clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Lst[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Lst[T]] = Show.derived
}

// A simple product type
case class Pair[T](x: T, y: T) // derives Eq, Pickler, Show

object Pair {
  // common compiler-generated infrastructure
  import TypeLevel.*

  type Shape[T] = Shape.Case[Pair[T], (T, T)]

  val reflectedClass = new ReflectedClass("Pair\000x\000y")
  import reflectedClass.mirror

  implicit def derived$Shape[T]: Shaped[Pair[T], Shape[T]] = new {
    def reflect(xy: Pair[T]) =
      mirror(0, xy)
    def reify(c: Mirror): Pair[T] =
      Pair(c(0).asInstanceOf, c(1).asInstanceOf)
    def common = reflectedClass
  }

  // clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Pair[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Pair[T]] = Show.derived
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

  val reflectedClass = new ReflectedClass("Left\000x\001Right\000x")
  import reflectedClass.mirror

  implicit def derived$Shape[L, R]: Shaped[Either[L, R], Shape[L, R]] = new {
    def reflect(e: Either[L, R]): Mirror = e match {
      case e: Left[L] => mirror(0, e)
      case e: Right[R] => mirror(1, e)
    }
    def reify(c: Mirror): Either[L, R] = c.ordinal match {
      case 0 => Left(c(0).asInstanceOf)
      case 1 => Right(c(0).asInstanceOf)
    }
    def common = reflectedClass
  }

  implicit def derived$Eq[L: Eq, R: Eq]: Eq[Either[L, R]] = Eq.derived
  implicit def derived$Pickler[L: Pickler, R: Pickler]: Pickler[Either[L, R]] = Pickler.derived
  implicit def derived$Show[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

// A type class
trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.{erasedValue, error, summonFrom}
  import TypeLevel.*

  inline def tryEql[T](x: T, y: T) = summonInline[Eq[T]].eql(x, y)

  inline def eqlElems[Elems <: Tuple](xm: Mirror, ym: Mirror, n: Int): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](xm(n).asInstanceOf, ym(n).asInstanceOf) &&
        eqlElems[elems1](xm, ym, n + 1)
      case _: EmptyTuple =>
        true
    }

  inline def eqlCase[T, Elems <: Tuple](r: Reflected[T], x: T, y: T) =
    eqlElems[Elems](r.reflect(x), r.reflect(y), 0)

  inline def eqlCases[T, Alts <: Tuple](xm: Mirror, ym: Mirror, ordinal: Int, n: Int): Boolean =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[alt, elems] *: alts1) =>
        inline typeOf[alt] match {
          case _: Subtype[T] =>
            if (n == ordinal) eqlElems[elems](xm, ym, 0)
            else eqlCases[T, alts1](xm, ym, ordinal, n + 1)
          case _ =>
            error("invalid call to eqlCases: one of Alts is not a subtype of T")
        }
      case _: EmptyTuple =>
        false
    }

  inline def eqlMain[T, S <: Shape](xm: Mirror, ym: Mirror): Boolean =
    inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        val ord = xm.ordinal
        ord == ym.ordinal &&
        eqlCases[T, alts](xm, ym, ord, 0)
      case _: Shape.Case[_, elems] =>
        eqlElems[elems](xm, ym, 0)
    }

  inline def derived[T, S <: Shape](implicit ev: Shaped[T, S]): Eq[T] = new {
    def eql(x: T, y: T): Boolean = eqlMain[T, S](ev.reflect(x), ev.reflect(y))
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }
}

// Another type class
trait Pickler[T] {
  def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
  def unpickle(buf: mutable.ListBuffer[Int]): T
}

object Pickler {
  import scala.compiletime.{erasedValue, constValue, error, summonInline}
  import TypeLevel.*

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = summonInline[Pickler[T]].pickle(buf, x)

  inline def pickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: Mirror, n: Int): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryPickle[elem](buf, elems(n).asInstanceOf[elem])
        pickleElems[elems1](buf, elems, n + 1)
      case _: EmptyTuple =>
    }

  inline def pickleCase[T, Elems <: Tuple](r: Reflected[T], buf: mutable.ListBuffer[Int], x: T): Unit =
    pickleElems[Elems](buf, r.reflect(x), 0)

  inline def pickleCases[T, Alts <: Tuple](r: Reflected[T], buf: mutable.ListBuffer[Int], x: T, n: Int): Unit =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[alt, elems] *: alts1) =>
        inline typeOf[alt] match {
          case _: Subtype[T] =>
            x match {
              case x: `alt` =>
                buf += n
                pickleCase[T, elems](r, buf, x)
              case _ =>
                pickleCases[T, alts1](r, buf, x, n + 1)
            }
          case _ =>
            error("invalid pickleCases call: one of Alts is not a subtype of T")
        }
      case _: EmptyTuple =>
    }

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = summonInline[Pickler[T]].unpickle(buf)

  inline def unpickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: Array[AnyRef], n: Int): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        elems(n) = tryUnpickle[elem](buf).asInstanceOf[AnyRef]
        unpickleElems[elems1](buf, elems, n + 1)
      case _: EmptyTuple =>
    }

  inline def unpickleCase[T, Elems <: Tuple](r: Reflected[T], buf: mutable.ListBuffer[Int], ordinal: Int): T = {
    inline val size = constValue[Tuple.Size[Elems]]
    inline if (size == 0)
      r.reify(r.common.mirror(ordinal))
    else {
      val elems = new Array[Object](size)
      unpickleElems[Elems](buf, elems, 0)
      r.reify(r.common.mirror(ordinal, elems))
    }
  }

  inline def unpickleCases[T, Alts <: Tuple](r: Reflected[T], buf: mutable.ListBuffer[Int], ordinal: Int, n: Int): T =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[_, elems] *: alts1) =>
        if (n == ordinal) unpickleCase[T, elems](r, buf, ordinal)
        else unpickleCases[T, alts1](r, buf, ordinal, n + 1)
      case _ =>
        throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ordinal")
    }

  inline def derived[T, S <: Shape](implicit ev: Shaped[T, S]): Pickler[T] = new {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit = inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        pickleCases[T, alts](ev, buf, x, 0)
      case _: Shape.Case[_, elems] =>
        pickleCase[T, elems](ev, buf, x)
    }
    def unpickle(buf: mutable.ListBuffer[Int]): T = inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        unpickleCases[T, alts](ev, buf, nextInt(buf), 0)
      case _: Shape.Case[_, elems] =>
        unpickleCase[T, elems](ev, buf, 0)
    }
  }

  implicit object IntPickler extends Pickler[Int] {
    def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
    def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
  }
}

// A third type class, making use of labels
trait Show[T] {
  def show(x: T): String
}
object Show {
  import scala.compiletime.{erasedValue, error, summonFrom}
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
              case x: `alt` =>
                showCase[T, elems](r, x)
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
  val eq = implicitly[Eq[Lst[Int]]]
  val xs = Lst.Cons(11, Lst.Cons(22, Lst.Cons(33, Lst.Nil)))
  val ys = Lst.Cons(11, Lst.Cons(22, Lst.Nil))
  assert(eq.eql(xs, xs))
  assert(!eq.eql(xs, ys))
  assert(!eq.eql(ys, xs))
  assert(eq.eql(ys, ys))

  val eq2 = implicitly[Eq[Lst[Lst[Int]]]]
  val xss = Lst.Cons(xs, Lst.Cons(ys, Lst.Nil))
  val yss = Lst.Cons(xs, Lst.Nil)
  assert(eq2.eql(xss, xss))
  assert(!eq2.eql(xss, yss))
  assert(!eq2.eql(yss, xss))
  assert(eq2.eql(yss, yss))

  val buf = new mutable.ListBuffer[Int]
  val pkl = implicitly[Pickler[Lst[Int]]]
  pkl.pickle(buf, xs)
  println(buf)
  val xs1 = pkl.unpickle(buf)
  println(xs1)
  assert(xs1 == xs)
  assert(eq.eql(xs1, xs))

  val pkl2 = implicitly[Pickler[Lst[Lst[Int]]]]
  pkl2.pickle(buf, xss)
  println(buf)
  val xss1 = pkl2.unpickle(buf)
  println(xss1)
  assert(xss == xss1)
  assert(eq2.eql(xss, xss1))

  val p1 = Pair(1, 2)
  val p2 = Pair(1, 2)
  val p3 = Pair(2, 1)
  val eqp = implicitly[Eq[Pair[Int]]]
  assert(eqp.eql(p1, p2))
  assert(!eqp.eql(p2, p3))

  val pklp = implicitly[Pickler[Pair[Int]]]
  pklp.pickle(buf, p1)
  println(buf)
  val p1a = pklp.unpickle(buf)
  println(p1a)
  assert(p1 == p1a)
  assert(eqp.eql(p1, p1a))

  def showPrintln[T: Show](x: T): Unit =
    println(implicitly[Show[T]].show(x))
  showPrintln(xs)
  showPrintln(xss)

  val zs = Lst.Cons(Left(1), Lst.Cons(Right(Pair(2, 3)), Lst.Nil))
  showPrintln(zs)

  def pickle[T: Pickler](buf: mutable.ListBuffer[Int], x: T): Unit =
    implicitly[Pickler[T]].pickle(buf, x)

  def unpickle[T: Pickler](buf: mutable.ListBuffer[Int]): T =
    implicitly[Pickler[T]].unpickle(buf)

  def copy[T: Pickler](x: T): T = {
    val buf = new mutable.ListBuffer[Int]
    pickle(buf, x)
    unpickle[T](buf)
  }

  def eql[T: Eq](x: T, y: T) = implicitly[Eq[T]].eql(x, y)

  val zs1 = copy(zs)
  showPrintln(zs1)
  assert(eql(zs, zs1))
}
