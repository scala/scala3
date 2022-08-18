import scala.deriving.Mirror
import scala.compiletime._

@main def Test: Unit = {

  object Parent {
    sealed trait Item
  }
  object Wrap {
    case object A extends Parent.Item
    case class B(i: Int, is: Parent.Item) extends Parent.Item
    case class C(i: Int, is: Parent.Item) extends Parent.Item
    case object C // force anon mirror
  }

  val M_Item = summon[Mirror.Of[Parent.Item]]
  assert(M_Item.ordinal(Wrap.A) == 0)
  assert(M_Item.ordinal(Wrap.B(1, Wrap.A)) == 1)
  assert(M_Item.ordinal(Wrap.C(2, Wrap.A)) == 2)

  val M_Wrap_B = summon[Mirror.Of[Tuple.Elem[M_Item.MirroredElemTypes, 1]]]
  assert(M_Wrap_B.fromProduct((1, Wrap.A)) == Wrap.B(1, Wrap.A))

  val M_Wrap_C = summon[Mirror.Of[Tuple.Elem[M_Item.MirroredElemTypes, 2]]]
  assert(M_Wrap_C.fromProduct((1, Wrap.A)) == Wrap.C(1, Wrap.A))
}