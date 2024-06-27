

import scala.annotation.switch

sealed trait Fruit

object Fruit {
  case object Apple extends Fruit
  case object Banana extends Fruit
  case object Lemon extends Fruit
  case object Lime extends Fruit
  case object Orange extends Fruit

  def isCitrus(fruit: Fruit): Boolean =
    (fruit: @switch) match {  // warn Could not emit switch for @switch annotated match
      case Orange => true
      case Lemon  => true
      case Lime   => true
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
    (fruit.tag: @switch) match {  // warn Could not emit switch for @switch annotated match
      case Apple.tag => true
      case 2         => true
      case 3         => true
      case _         => false
  }

  // fewer than four cases, so no warning
  def succ1(fruit: TaggedFruit): Boolean =
    (fruit.tag: @switch) match {
      case 3             => false
      case 2 | Apple.tag => true
    }

  // fewer than four cases, so no warning
  def succ2(fruit: TaggedFruit): Boolean =
    (fruit.tag: @switch) match {
      case 3          => false
      case 2          => true
      case Apple.tag  => true
  }
}