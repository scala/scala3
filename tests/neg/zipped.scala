object Test {
  val xs: List[Int] = ???

  xs.lazyZip(xs).lazyZip(xs)
    .map( (x: (Int, Int, Int)) => x match { case (x, y, z) => x + y + z })     // ok

  xs.lazyZip(xs).lazyZip(xs)
    .map( x => x match { case (x, y, z) => x + y + z })     // error
}