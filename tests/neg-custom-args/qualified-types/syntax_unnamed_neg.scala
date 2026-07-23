case class Box[T](x: T)
def id[T](x: T): T = x

abstract class Test:
  val v1: Box[Int with v1 > 0] // error: Cyclic reference
  val v2: {v: Int with id[Int with v2 > 0](???) > 0} // error: Cyclic reference
  val v3: {v: Int with (??? : Int with v3 == 2) > 0} // error: Cyclic reference
