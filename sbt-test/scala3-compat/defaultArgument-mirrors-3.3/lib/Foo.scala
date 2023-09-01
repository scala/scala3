package lib

import deriving.Mirror

case class OldFoo(x: Int = 1, y: Int)

case class OldBar(x: Int = 1, y: Int)
case object OldBar

object OldMirrors {
  val mOldFoo = summon[Mirror.ProductOf[OldFoo]]
  val mOldBar = summon[Mirror.ProductOf[OldBar]]
}
