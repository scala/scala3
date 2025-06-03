package cyclicObject

object O1 { // warn
  val o = cyclicObject.O2
}

object O2 {
  val o = cyclicObject.O1
}
