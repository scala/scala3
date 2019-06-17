object Test {
  val xs: List[?] = List(1, 2, 3)
  val ys: Map[? <: AnyRef, ? >: Null] = Map()
}