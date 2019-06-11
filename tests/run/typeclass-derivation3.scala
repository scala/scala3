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
  sealed trait Either[+L, +R] extends Product with Serializable derives Eq, Pickler, Show
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
    import scala.deriving._

    inline def tryEql[TT](x: TT, y: TT): Boolean = delegate match {
      case eq: Eq[TT] => eq.eql(x, y)
    }

    inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
          eqlElems[elems1](n + 1)(x, y)
        case _: Unit =>
          true
      }

    inline def eqlProduct[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
      eqlElems[m.MirroredElemTypes](0)(x, y)

    inline def eqlCases[Alts](n: Int)(x: Any, y: Any, ord: Int): Boolean =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt`] => eqlElems[m.MirroredElemTypes](0)(x, y)
            }
          else eqlCases[alts1](n + 1)(x, y, ord)
        case _: Unit =>
          false
      }

    inline def derived[T](implicit ev: Mirror.Of[T]): Eq[T] = new Eq[T] {
      def eql(x: T, y: T): Boolean =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            ord == m.ordinal(y) && eqlCases[m.MirroredElemTypes](0)(x, y, ord)
          case m: Mirror.ProductOf[T] =>
            eqlElems[m.MirroredElemTypes](0)(x, y)
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
    import deriving._

    def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

    inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = delegate match {
      case pkl: Pickler[T] => pkl.pickle(buf, x)
    }

    inline def pickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any): Unit =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryPickle[elem](buf, productElement[elem](x, n))
          pickleElems[elems1](n + 1)(buf, x)
        case _: Unit =>
      }

    inline def pickleCases[Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any, ord: Int): Unit =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt`] => pickleElems[m.MirroredElemTypes](0)(buf, x)
            }
          else pickleCases[alts1](n + 1)(buf, x, ord)
        case _: Unit =>
      }

    inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = delegate match {
      case pkl: Pickler[T] => pkl.unpickle(buf)
    }

    inline def unpickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], elems: ArrayProduct): Unit =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          elems(n) = tryUnpickle[elem](buf).asInstanceOf[AnyRef]
          unpickleElems[elems1](n + 1)(buf, elems)
        case _: Unit =>
      }

    inline def unpickleCase[T, Elems <: Tuple](buf: mutable.ListBuffer[Int], m: Mirror.ProductOf[T]): T = {
      inline val size = constValue[Tuple.Size[Elems]]
      inline if (size == 0)
        m.fromProduct(EmptyProduct)
      else {
        val elems = new ArrayProduct(size)
        unpickleElems[Elems](0)(buf, elems)
        m.fromProduct(elems)
      }
    }

    inline def unpickleCases[T, Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], ord: Int): T =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt` & T] =>
                unpickleCase[`alt` & T, m.MirroredElemTypes](buf, m)
            }
          else unpickleCases[T, alts1](n + 1)(buf, ord)
        case _: Unit =>
          throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ord")
      }

    inline def derived[T](implicit ev: Mirror.Of[T]): Pickler[T] = new {
      def pickle(buf: mutable.ListBuffer[Int], x: T): Unit =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            buf += ord
            pickleCases[m.MirroredElemTypes](0)(buf, x, ord)
          case m: Mirror.ProductOf[T] =>
            pickleElems[m.MirroredElemTypes](0)(buf, x)
        }
      def unpickle(buf: mutable.ListBuffer[Int]): T =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = nextInt(buf)
            unpickleCases[T, m.MirroredElemTypes](0)(buf, ord)
          case m: Mirror.ProductOf[T] =>
            unpickleCase[T, m.MirroredElemTypes](buf, m)
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
    import deriving._

    inline def tryShow[T](x: T): String = delegate match {
      case s: Show[T] => s.show(x)
    }

    inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Any): List[String] =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          inline erasedValue[Labels] match {
            case _: (label *: labels1) =>
              val formal = constValue[label]
              val actual = tryShow(productElement[elem](x, n))
              s"$formal = $actual" :: showElems[elems1, labels1](n + 1)(x)
          }
        case _: Unit =>
          Nil
    }

    inline def showCase[T](x: Any, m: Mirror.ProductOf[T]): String = {
      val label = constValue[m.MirroredLabel]
      inline m match {
        case m: Mirror.Singleton => label
        case _ => showElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(x).mkString(s"$label(", ", ", ")")
      }
    }

    inline def showCases[Alts <: Tuple](n: Int)(x: Any, ord: Int): String =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            delegate match {
              case m: Mirror.ProductOf[`alt`] =>
                showCase(x, m)
            }
          else showCases[alts1](n + 1)(x, ord)
        case _: Unit =>
          throw new MatchError(x)
      }

    inline def derived[T](implicit ev: Mirror.Of[T]): Show[T] = new {
      def show(x: T): String =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            showCases[m.MirroredElemTypes](0)(x, ord)
          case m: Mirror.ProductOf[T] =>
            showCase(x, m)
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
