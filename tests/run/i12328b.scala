import scala.deriving.Mirror
import scala.compiletime._

@main def Test: Unit = {

  sealed trait Item
  case object A extends Item
  case class B(i: Int, is: Item) extends Item
  case class C(i: Int, is: Item) extends Item
  case object C // force anon mirror

  val M_Item = summon[Mirror.Of[Item]]
  assert(M_Item.ordinal(A) == 0)
  assert(M_Item.ordinal(B(1, A)) == 1)
  assert(M_Item.ordinal(C(2, A)) == 2)

  val M_B = summon[Mirror.Of[Tuple.Elem[M_Item.MirroredElemTypes, 1]]]
  assert(M_B.fromProduct((1, A)) == B(1, A))

  val M_C = summon[Mirror.Of[Tuple.Elem[M_Item.MirroredElemTypes, 2]]]
  assert(M_C.fromProduct((1, A)) == C(1, A))
}