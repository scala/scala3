import scala.collection.mutable
import scala.annotation.tailrec

object datatypes {
  import typeclasses._
  // An algebraic datatype
  enum Lst[+T] derives Eq, Pickler, Show {
    case Cons(hd: T, tl: Lst[T])
    case Nil
  }

  object Lst {
    // common compiler-generated infrastructure
    import typelevel._
/*
    type Shape[T] = Shape.Cases[(
      Shape.Case[Cons[T], (T, Lst[T])],
      Shape.Case[Nil.type, Unit]
    )]
*/
    val reflectedClass = new ReflectedClass(Array("Cons\000hd\000tl", "Nil"))
    import reflectedClass.mirror

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
      def common = reflectedClass
    }

    // three clauses that could be generated from a `derives` clause
    implicit def LstEq[T: Eq]: Eq[Lst[T]] = Eq.derived
    implicit def LstPickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
    implicit def LstShow[T: Show]: Show[Lst[T]] = Show.derived
  }

  // A simple product type
  case class Pair[T](x: T, y: T) derives Eq, Pickler, Show

  object Pair {
    // common compiler-generated infrastructure
    import typelevel._

  //  type Shape[T] = Shape.Case[Pair[T], (T, T)]

    val reflectedClass = new ReflectedClass(Array("Pair\000x\000y"))
    import reflectedClass.mirror

    implicit def pairShape[T]: Shaped[Pair[T], Shape[T]] = new {
      def reflect(xy: Pair[T]) =
        mirror(0, xy)
      def reify(c: Mirror): Pair[T] =
        Pair(c(0).asInstanceOf, c(1).asInstanceOf)
      def common = reflectedClass
    }

    // two clauses that could be generated from a `derives` clause
    implicit def PairEq[T: Eq]: Eq[Pair[T]] = Eq.derived
    implicit def PairPickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
    implicit def PairShow[T: Show]: Show[Pair[T]] = Show.derived
  }

  sealed trait Either[+L, +R] extends Product derives Eq, Pickler, Show
  case class Left[L](x: L) extends Either[L, Nothing]
  case class Right[R](x: R) extends Either[Nothing, R]

  object Either {
    import typelevel._

  /*
    type Shape[L, R] = Shape.Cases[(
      Shape.Case[Left[L], L *: Unit],
      Shape.Case[Right[R], R *: Unit]
    )]
*/
    val reflectedClass = new ReflectedClass(Array("Left\000x", "Right\000x"))
    import reflectedClass.mirror

    implicit def eitherShape[L, R]: Shaped[Either[L, R], Shape[L, R]] = new {
      def reflect(e: Either[L, R]): Mirror = e match {
        case e: Left[L] => mirror(0, e)
        case e: Right[R] => mirror(1, e)
      }
      def reify(c: Mirror): Either[L, R] = c.ordinal match {
        case 0 => Left[L](c(0).asInstanceOf)
        case 1 => Right[R](c(0).asInstanceOf)
      }
      def common = reflectedClass
    }

    implicit def EitherEq[L: Eq, R: Eq]: Eq[Either[L, R]] = Eq.derived
    implicit def EitherPickler[L: Pickler, R: Pickler]: Pickler[Either[L, R]] = Pickler.derived
    implicit def EitherShow[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
  }
}

object typeclasses {
  // A typeclass
  trait Eq[T] {
    def eql(x: T, y: T): Boolean
  }

  object Eq {
    import scala.typelevel.erasedValue
    import typelevel._

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

    inline def eqlCase[T, Elems <: Tuple](r: Reflected[T], x: T, y: T) =
      eqlElems[Elems](r.reflect(x), r.reflect(y), 0)

    inline def eqlCases[T, Alts <: Tuple](r: Reflected[T], x: T, y: T): Boolean =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[alt, elems] *: alts1) =>
          x match {
            case x: `alt` =>
              y match {
                case y: `alt` => eqlCase[T, elems](r, x, y)
                case _ => false
              }
            case _ => eqlCases[T, alts1](r, x, y)
        }
      case _: Unit =>
        false
    }

    inline def derived[T, S <: Shape](implicit ev: Shaped[T, S]): Eq[T] = new {
      def eql(x: T, y: T): Boolean = inline erasedValue[S] match {
        case _: Shape.Cases[alts] =>
          eqlCases[T, alts](ev, x, y)
        case _: Shape.Case[_, elems] =>
          eqlCase[T, elems](ev, x, y)
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
    import scala.typelevel.{erasedValue, constValue}
    import typelevel._

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

    inline def pickleCase[T, Elems <: Tuple](r: Reflected[T], buf: mutable.ListBuffer[Int], x: T): Unit =
      pickleElems[Elems](buf, r.reflect(x), 0)

    inline def pickleCases[T, Alts <: Tuple](r: Reflected[T], buf: mutable.ListBuffer[Int], x: T, n: Int): Unit =
      inline erasedValue[Alts] match {
        case _: (Shape.Case[alt, elems] *: alts1) =>
          x match {
            case x: `alt` =>
              buf += n
              pickleCase[T, elems](r, buf, x)
            case _ =>
              pickleCases[T, alts1](r, buf, x, n + 1)
          }
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

  // A third typeclass, making use of labels
  trait Show[T] {
    def show(x: T): String
  }
  object Show {
    import scala.typelevel.erasedValue
    import typelevel._

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
}