object test {
  val b: scala.Boolean = null                         // error
  val c: Unit = null
  val d: Float = null                                 // error
  val e: AnyVal = null                                // ok under -Yexplicit-nulls: Null <: AnyVal
}
