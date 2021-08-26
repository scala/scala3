import scala.deriving.Mirror

@main def Test: Unit = {

  sealed trait Item

  object Item {
    case object A extends Item
    case object B extends Item
  }

  val mItem = summon[Mirror.Of[Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(Item.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == Item.A)

}
