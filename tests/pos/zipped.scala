// This test shows some un-intuitive behavior of the `zipped` method.
object Test {
  val xs: List[Int] = ???

  // 1. This works, since withFilter is not defined on Tuple3zipped. Instead,
  // an implicit conversion from Tuple3zipped to Traversable[(Int, Int, Int)] is inserted.
  // The subsequent map operation has the right type for this Traversable.
  xs.lazyZip(xs).lazyZip(xs)
    .withFilter( (x: (Int, Int, Int)) => x match { case (x, y, z) => true } )  // OK
    .map( (x: (Int, Int, Int)) => x match { case (x, y, z) => x + y + z })     // OK


  // 2. This works as well, because of auto untupling i.e. `case` is inserted.
  // But it does not work in Scala2.
  xs.lazyZip(xs).lazyZip(xs)
  .withFilter( (x: (Int, Int, Int)) => x match { case (x, y, z) => true } )  // OK
  .map( (x: Int, y: Int, z: Int) => x + y + z )  // OK
      // works, because of auto untupling i.e. `case` is inserted
      // does not work in Scala2

  // 3. Now, without withFilter, it's the opposite, we need the 3 parameter map.
  xs.lazyZip(xs).lazyZip(xs)
    .map( (x: Int, y: Int, z: Int) => x + y + z )     // OK

  // 4. The single parameter map works through an inserted conversion
  //xs.lazyZip(xs).lazyZip(xs)
  //  .map( (x: (Int, Int, Int)) => x match { case (x, y, z) => x + y + z })     // now also OK

  // 5. If we leave out the parameter type, it now works as well.
  //xs.lazyZip(xs).lazyZip(xs)
  //  .map( x => x match { case (x, y, z) => x + y + z })     // now also OK

  // This means that the following works in Dotty 3.0 as well as 3.x
  for ((x, y, z) <- xs.lazyZip(xs).lazyZip(xs)) yield x + y + z
}