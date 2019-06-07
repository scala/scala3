import scala.collection.mutable
import scala.annotation.tailrec

// The following command:
//
//     sc typeclass-scaling.scala -Xmax-inlines 100 -Xprint:front -color:never -pagewidth 1000 >& x
//
// produces an output file with `wc` measures (lines/words/chars):
//
//   89327  162884 7220258
//
// The command
//
//     time sc typeclass-scaling.scala -Xmax-inlines 100
//
// gives (best of three):
//
// real	0m16.593s
// user	1m6.337s
// sys	0m1.344s
object datatypes {
  import typeclasses._

  enum E1[T] derives Eq, Pickler {
    case C1(x1: T)
  }

  enum E2[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
  }

  enum E3[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
  }

  enum E4[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
  }

  enum E5[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
  }

  enum E6[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
  }

  enum E7[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
  }

  enum E8[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
  }

  enum E9[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
  }

  enum E10[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
  }

  enum E11[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
  }

  enum E12[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
    case C12(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T)
  }

  enum E13[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
    case C12(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T)
    case C13(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T)
  }

  enum E14[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
    case C12(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T)
    case C13(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T)
    case C14(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T)
  }

  enum E15[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
    case C12(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T)
    case C13(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T)
    case C14(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T)
    case C15(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T, x15: T)
  }

  enum E16[T] derives Eq, Pickler {
    case C1(x1: T)
    case C2(x1: T, x2: T)
    case C3(x1: T, x2: T, x3: T)
    case C4(x1: T, x2: T, x3: T, x4: T)
    case C5(x1: T, x2: T, x3: T, x4: T, x5: T)
    case C6(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T)
    case C7(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T)
    case C8(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T)
    case C9(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T)
    case C10(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T)
    case C11(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T)
    case C12(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T)
    case C13(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T)
    case C14(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T)
    case C15(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T, x15: T)
    case C16(x1: T, x2: T, x3: T, x4: T, x5: T, x6: T, x7: T, x8: T, x9: T, x10: T, x11: T, x12: T, x13: T, x14: T, x15: T, x16: T)
  }
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

    inline def tryEql[TT](x: TT, y: TT): Boolean = implied match {
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
            implied match {
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

    inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = implied match {
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
            implied match {
              case m: Mirror.ProductOf[`alt`] => pickleElems[m.MirroredElemTypes](0)(buf, x)
            }
          else pickleCases[alts1](n + 1)(buf, x, ord)
        case _: Unit =>
      }

    inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = implied match {
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
            implied match {
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
}
import datatypes._
import typeclasses._

// Tests
object Test extends App {
  implicitly[Eq[E1[Int]]]
  implicitly[Eq[E2[Int]]]
  implicitly[Eq[E3[Int]]]
  implicitly[Eq[E4[Int]]]
  implicitly[Eq[E5[Int]]]
  implicitly[Eq[E6[Int]]]
  implicitly[Eq[E7[Int]]]
  implicitly[Eq[E8[Int]]]
  implicitly[Eq[E9[Int]]]
  implicitly[Eq[E10[Int]]]
  implicitly[Eq[E11[Int]]]
  implicitly[Eq[E12[Int]]]
  implicitly[Eq[E13[Int]]]
  implicitly[Eq[E14[Int]]]
  implicitly[Eq[E15[Int]]]
  implicitly[Eq[E16[Int]]]
  implicitly[Pickler[E1[Int]]]
  implicitly[Pickler[E2[Int]]]
  implicitly[Pickler[E3[Int]]]
  implicitly[Pickler[E4[Int]]]
  implicitly[Pickler[E5[Int]]]
  implicitly[Pickler[E6[Int]]]
  implicitly[Pickler[E7[Int]]]
  implicitly[Pickler[E8[Int]]]
  implicitly[Pickler[E9[Int]]]
  implicitly[Pickler[E10[Int]]]
  implicitly[Pickler[E11[Int]]]
  implicitly[Pickler[E12[Int]]]
  implicitly[Pickler[E13[Int]]]
  implicitly[Pickler[E14[Int]]]
  implicitly[Pickler[E15[Int]]]
  implicitly[Pickler[E16[Int]]]
}