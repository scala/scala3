import scala.deriving.Mirror

class Outer { self =>

  object Inner {
    sealed trait Item { thisItem => // both children and parent share a common sub-prefix

      val mItem = summon[Mirror.Of[thisItem.type]] // error: Outer.Inner.A.type is not a subtype of thisItem.type

    }

    case object A extends self.Inner.Item
    case object B extends self.Inner.Item
  }

}
