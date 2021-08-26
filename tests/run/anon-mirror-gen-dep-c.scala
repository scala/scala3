import scala.deriving.Mirror

class Outer {

  sealed trait Item // start of children's prefix is identical to parent prefix (anon mirror)

  object Wrapper {
    case object A extends Item
    case object B extends Item
  }

}

@main def Test: Unit = {

  val o = new Outer()

  val mItem = summon[Mirror.Of[o.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(o.Wrapper.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == o.Wrapper.A)

}
