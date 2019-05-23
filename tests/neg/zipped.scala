// This test shows some un-intuitive behavior of the `zipped` method.
object Test {
  val xs: List[Int] = ???

  // 1. This works, since withFilter is not defined on Tuple3zipped. Instead,
  // an implicit conversion from Tuple3zipped to Traversable[(Int, Int, Int)] is inserted.
  // The subsequent map operation has the right type for this Traversable.
  (xs, xs, xs).zipped
    .withFilter( (x: (Int, Int, Int)) => x match { case (x, y, z) => true } )  // OK
    .map( (x: (Int, Int, Int)) => x match { case (x, y, z) => x + y + z })     // OK


  // 2. This works as well, because of auto untupling i.e. `case` is inserted.
  // But it does not work in Scala2.
  (xs, xs, xs).zipped
  .withFilter( (x: (Int, Int, Int)) => x match { case (x, y, z) => true } )  // OK
  .map( (x: Int, y: Int, z: Int) => x + y + z )  // OK
      // works, because of auto untupling i.e. `case` is inserted
      // does not work in Scala2

  // 3. Now, without withFilter, it's the opposite, we need the 3 parameter map.
  (xs, xs, xs).zipped
    .map( (x: Int, y: Int, z: Int) => x + y + z )     // OK

  // 4. The single parameter map does not work.
  (xs, xs, xs).zipped
    .map( (x: (Int, Int, Int)) => x match { case (x, y, z) => x + y + z })     // error

  // 5. If we leave out the parameter type, we get a "Wrong number of parameters" error instead
  (xs, xs, xs).zipped
    .map( x => x match { case (x, y, z) => x + y + z })     // error

  // This means that the following works in Dotty in normal mode, since a `withFilter`
  // is inserted. But it does no work under -strict. And it will not work in Scala 3.1.
  // The reason is that without -strict, the code below is mapped to (1), but with -strict
  // it is mapped to (5).
  for ((x, y, z) <- (xs, xs, xs).zipped) yield x + y + z
}