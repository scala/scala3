import scala.deriving.Mirror

class Outer {

  object Inner {

    sealed trait Item

    object Wrapper {
      case object A extends Item
      case object B extends Item
    }

  }

}

@main def Test: Unit = {

  val o = new Outer()

  val mItem = summon[Mirror.Of[o.Inner.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(o.Inner.Wrapper.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == o.Inner.Wrapper.A)

}
