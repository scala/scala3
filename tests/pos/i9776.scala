//> using options -Werror -deprecation -feature

import scala.annotation.switch

sealed trait Fruit

object Fruit {
  case object Apple extends Fruit
  case object Banana extends Fruit
  case object Orange extends Fruit

  def isCitrus(fruit: Fruit): Boolean =
    (fruit: @switch) match {
      case Orange => true
      case _      => false
    }
}


sealed trait TaggedFruit {
  def tag: Int
}

object TaggedFruit {
  case object Apple extends TaggedFruit {
    val tag = 1
  }
  case object Banana extends TaggedFruit {
    val tag = 2
  }
  case object Orange extends TaggedFruit {
    val tag = 3
  }

  def isCitrus(fruit: TaggedFruit): Boolean =
    (fruit.tag: @switch) match {
      case 3 => true
      case _ => false
  }
}
