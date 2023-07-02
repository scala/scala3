object Test {
  val pv0: [T] => List[T] = ???        // error
  val pv1: Any = [T] => Nil            // error
  val pv2: [T] => List[T] = [T] => Nil // error // error

  val intraDep = [T] => (x: T, y: List[x.type]) => List(y) // error
}
