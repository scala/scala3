import scala.deriving.Mirror

class Outer {

  sealed trait Item

  object Wrapper {
    case object A extends Item
    case object B extends Item
  }

}

@main def Test = {
  val mItem = summon[Mirror.Of[Outer#Item]] // error
}
