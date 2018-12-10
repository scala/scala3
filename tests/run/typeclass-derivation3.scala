import scala.collection.mutable
import scala.annotation.tailrec

object datatypes {
  import typeclasses._

  // An algebraic datatype
  enum Lst[+T] derives Eq, Pickler, Show {
    case Cons(hd: T, tl: Lst[T])
    case Nil
  }
  object Lst {}


  // A simple product type
  case class Pair[T](x: T, y: T) derives Eq, Pickler, Show

  // another ADT
  sealed trait Either[+L, +R] extends Product derives Eq, Pickler, Show
  case class Left[L](x: L) extends Either[L, Nothing]
  case class Right[R](x: R) extends Either[Nothing, R]
}

object typeclasses {
  // A typeclass
  trait Eq[T] {
    def eql(x: T, y: T): Boolean
  }

  object Eq {
    import scala.compiletime.erasedValue
    import compiletime._
    import reflect.{Mirror, Generic}

    inline def tryEql[T](x: T, y: T) = implicit match {
      case eq: Eq[T] => eq.eql(x, y)
    }

    inline def eqlElems[Elems <: Tuple](xs: Mirror, ys: Mirror, n: Int): Boolean =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryEql[elem](xs(n).asInstanceOf, ys(n).asInstanceOf) &&
          eqlElems[elems1](xs, ys, n + 1)
        case _: Unit =>
          true
      }

    inline def eqlCases[Alts <: Tuple](xm: Mirror, ym: Mirror, n: Int): Boolean =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[alt, elems] *: alts1) =>
          if (xm.ordinal == n) eqlElems[elems](xm, ym, 0)
          else eqlCases[alts1](xm, ym, n + 1)
      case _: Unit =>
          false
      }

    inline def derived[T](implicit ev: Generic[T]): Eq[T] = new {
      def eql(x: T, y: T): Boolean = {
        val xm = ev.reflect(x)
        val ym = ev.reflect(y)
        inline erasedValue[ev.Shape] match {
          case _: Shape.Cases[alts] =>
            xm.ordinal == ym.ordinal &&
            eqlCases[alts](xm, ym, 0)
          case _: Shape.Case[_, elems] =>
            eqlElems[elems](xm, ym, 0)
        }
      }
    }

    implicit object IntEq extends Eq[Int] {
      def eql(x: Int, y: Int) = x == y
    }
  }

  // Another typeclass
  trait Pickler[T] {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
    def unpickle(buf: mutable.ListBuffer[Int]): T
  }

  object Pickler {
    import scala.compiletime.{erasedValue, constValue}
    import compiletime._
    import reflect.{Mirror, Generic}

    def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

    inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = implicit match {
      case pkl: Pickler[T] => pkl.pickle(buf, x)
    }

    inline def pickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: Mirror, n: Int): Unit =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryPickle[elem](buf, elems(n).asInstanceOf[elem])
          pickleElems[elems1](buf, elems, n + 1)
        case _: Unit =>
      }

    inline def pickleCases[Alts <: Tuple](buf: mutable.ListBuffer[Int], xm: Mirror, n: Int): Unit =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[alt, elems] *: alts1) =>
          if (xm.ordinal == n) pickleElems[elems](buf, xm, 0)
          else pickleCases[alts1](buf, xm, n + 1)
        case _: Unit =>
      }

    inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = implicit match {
      case pkl: Pickler[T] => pkl.unpickle(buf)
    }

    inline def unpickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: Array[AnyRef], n: Int): Unit =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          elems(n) = tryUnpickle[elem](buf).asInstanceOf[AnyRef]
          unpickleElems[elems1](buf, elems, n + 1)
        case _: Unit =>
      }

    inline def unpickleCase[T, Elems <: Tuple](gen: Generic[T], buf: mutable.ListBuffer[Int], ordinal: Int): T = {
      inline val size = constValue[Tuple.Size[Elems]]
      inline if (size == 0)
        gen.reify(gen.common.mirror(ordinal))
      else {
        val elems = new Array[Object](size)
        unpickleElems[Elems](buf, elems, 0)
        gen.reify(gen.common.mirror(ordinal, elems))
      }
    }

    inline def unpickleCases[T, Alts <: Tuple](gen: Generic[T], buf: mutable.ListBuffer[Int], ordinal: Int, n: Int): T =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[_, elems] *: alts1) =>
          if (n == ordinal) unpickleCase[T, elems](gen, buf, ordinal)
          else unpickleCases[T, alts1](gen, buf, ordinal, n + 1)
        case _ =>
          throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ordinal")
      }

    inline def derived[T](implicit ev: Generic[T]): Pickler[T] = new {
      def pickle(buf: mutable.ListBuffer[Int], x: T): Unit = {
        val xm = ev.reflect(x)
        inline erasedValue[ev.Shape] match {
          case _: Shape.Cases[alts] =>
            buf += xm.ordinal
            pickleCases[alts](buf, xm, 0)
          case _: Shape.Case[_, elems] =>
            pickleElems[elems](buf, xm, 0)
        }
      }
      def unpickle(buf: mutable.ListBuffer[Int]): T =
        inline erasedValue[ev.Shape] match {
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

  // A third typeclass, making use of labels
  trait Show[T] {
    def show(x: T): String
  }
  object Show {
    import scala.compiletime.erasedValue
    import compiletime._
    import reflect.{Mirror, Generic}

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

    inline def showCases[Alts <: Tuple](xm: Mirror, n: Int): String =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[alt, elems] *: alts1) =>
          if (xm.ordinal == n) showElems[elems](xm, 0).mkString(", ")
          else showCases[alts1](xm, n + 1)
        case _: Unit =>
          throw new MatchError(xm)
      }

    inline def derived[T](implicit ev: Generic[T]): Show[T] = new {
      def show(x: T): String = {
        val xm = ev.reflect(x)
        val args = inline erasedValue[ev.Shape] match {
          case _: Shape.Cases[alts] =>
            showCases[alts](xm, 0)
          case _: Shape.Case[_, elems] =>
            showElems[elems](xm, 0).mkString(", ")
        }
        s"${xm.caseLabel}($args)"
      }
    }

    implicit object IntShow extends Show[Int] {
      def show(x: Int): String = x.toString
    }
  }
}
import datatypes._
import typeclasses._

// Tests
object Test extends App {
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

  import scala.reflect.Generic

  val listGen = implicitly[Generic[scala.collection.immutable.List[Int]]]
  implicit def listEq[T: Eq]: Eq[List[T]] = Eq.derived
  val leq = implicitly[Eq[List[Int]]]
  println(leq.eql(List(1, 2, 3), List(1, 2, 3)))

  implicit def listShow[T: Show]: Show[List[T]] = Show.derived
  println(implicitly[Show[List[Int]]].show(List(1, 2, 3)))
  println(implicitly[Show[List[List[Int]]]].show(List(List(1), List(2, 3))))

  implicit def listPickler[T: Pickler]: Pickler[List[T]] = Pickler.derived
  val pklList = implicitly[Pickler[List[List[Int]]]]
  val zss = List(Nil, List(1), List(2, 3))
  pklList.pickle(buf, zss)
  val zss1 = pklList.unpickle(buf)
  assert(eql(zss, zss1))
  showPrintln(zss1)
}