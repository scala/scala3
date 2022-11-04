import scala.deriving.Mirror

trait Mixin
object Mixin

trait Parent
object Parent

sealed trait Fruit extends Parent
object Fruit {
  case object Apple extends Fruit
  case object Orange extends Fruit
}

@main def Test = {
  summon[Mirror.SumOf[Fruit & Mixin]] // error: not a sum type
}
