object Test {
  val xs: List[_] = List(1, 2, 3)  // error
  val ys: Map[_ <: AnyRef, _ >: Null] = Map() // error // error
}