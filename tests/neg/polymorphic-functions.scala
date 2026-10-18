object Test {
  val pv1: Any = [T] => Nil            // error
  val pv2: [T] => List[T] = [T] => Nil // error

  val intraDep = [T] => (x: T, y: List[x.type]) => List(y) // error
}
