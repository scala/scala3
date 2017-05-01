import dotty.{Tuple, TupleCons, LargeTuple}
import dotty.{TupleCons => ::}

trait Appender[L1 <: Tuple, L2 <: Tuple, Out <: Tuple] {
  def apply(l1: L1, l2: L2): Out
}

object Appender {
  implicit def caseUnit[L <: Tuple]: Appender[Unit, L, L] =
    new Appender[Unit, L, L] {
      def apply(l1: Unit, l2: L): L = l2
    }

  implicit def caseTupleCons[H, T <: Tuple, L <: Tuple, O <: Tuple]
    (implicit a: Appender[T, L, O]): Appender[H :: T, L, H :: O] =
      new Appender[H :: T, L, H :: O] {
        def apply(l1: H :: T, l2: L): H :: O = {
          l1 match {
            case TupleCons(head, tail) =>
              TupleCons(head, a(tail, l2))
          }
        }
      }
}

trait PhantomAppender[L1 <: Tuple, L2 <: Tuple, Out <: Tuple]
object PhantomAppender {
  implicit def caseUnit[L <: Tuple]: PhantomAppender[Unit, L, L] = null
  implicit def caseTupleCons[H, T <: Tuple, L <: Tuple, Out <: Tuple]
    (implicit p: PhantomAppender[T, L, Out]): PhantomAppender[H :: T, L, H :: Out] = null
}

// Syntax -----------------------------------------------------------------------------------------

object syntax {
  object append {
    implicit class TupleAppender[L1 <: Tuple](l1: L1) {
      def ++[L2 <: Tuple, Out <: Tuple](l2: L2)(implicit a: Appender[L1, L2, Out]): Out = a(l1, l2)
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    import syntax.append._

    val l1: (String, Boolean) =
      ("s", true)

    val l2: (Double, Double, Double) =
      (1d, 2d, 3d)

    val l3: (String, Boolean, Double, Double, Double) =
      l1 ++ l2

    assert(l3 == ("s", true, 1d, 2d, 3d))
  }
}
