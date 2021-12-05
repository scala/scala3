object Test {
  val xs: List[Int] = ???

 // Does not work anynore since auto(un)tupling does not work after conversions
  xs.lazyZip(xs).lazyZip(xs)
    .map( (x: (Int, Int, Int)) => x match { case (x, y, z) => x + y + z })     // error

  xs.lazyZip(xs).lazyZip(xs)
    .map( x => x match { case (x, y, z) => x + y + z })     // error
}