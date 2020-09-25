import Nat._, Lst._

def enumLabelOf[E](e: E)(using s: deriving.Mirror.SumOf[E]): String = s.enumLabel(e)
def ordinalOf[E](e: E)(using s: deriving.Mirror.SumOf[E]): Int = s.ordinal(e)

enum Nat:
  case S(pred: Nat)
  case Z

sealed abstract class Lst[+T]
object Lst:
  final case class Cdr[+T](t: T, ts: Lst[T]) extends Lst[T]
  case object NIL                            extends Lst[Nothing]

@main def Test =

  // labels
  assert(enumLabelOf(S(Z))                  == "S")
  assert(enumLabelOf(Z)                     == "Z")
  assert(enumLabelOf(Cdr(1, NIL): Lst[Int]) == "Cdr")
  assert(enumLabelOf(NIL: Lst[Int])         == "NIL")

  // ordinals
  assert(ordinalOf(S(Z))                    == 0)
  assert(ordinalOf(Z)                       == 1)
  assert(ordinalOf(Cdr(1, NIL): Lst[Int])   == 0)
  assert(ordinalOf(NIL: Lst[Int])           == 1)
