import scala.deriving.Mirror

class Outer {

  sealed trait Item

  object Wrapper {
    case object A extends Item
    case object B extends Item
  }

  class Inaccessible {
    case object C extends Item
  }

}

@main def hello: Unit = {

  val o = new Outer()

  val mItem = summon[Mirror.Of[o.Item]] // error
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]] // error

}
