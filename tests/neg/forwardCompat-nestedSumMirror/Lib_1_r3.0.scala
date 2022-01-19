// Adapted from i11050

sealed trait TreeValue

sealed trait SubLevel extends TreeValue

case class Leaf1(value: String) extends TreeValue
case class Leaf2(value: Int)    extends SubLevel
