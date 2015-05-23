


import collection._



object Test extends dotty.runtime.LegacyApp {

  val list: List[Int] = (immutable.Vector(1, 2, 3) :+ 4)(breakOut)

}
