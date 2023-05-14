import scala.deriving.Mirror

class Outer1 {

  sealed trait Item

  object Wrapper {
    case object A extends Item
  }

  class Inaccessible {
    case object B extends Item
  }

  val M = summon[Mirror.Of[Item]] // error: B is not accessible from Item

}

class Outer2 {
  sealed trait Item
  case object A extends Item
}

def testOuter2 =
  // discussion point: should we allow this, or only allow singleton prefix?
  val m_Outer2_Item = summon[Mirror.Of[Outer2#Item]] // error: Item is not accessible from m_Outer2_Item