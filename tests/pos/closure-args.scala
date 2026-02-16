import language.experimental.relaxedLambdaSyntax

val z = List(1).map: + => // ok
  ???

val xs = List(1)
val b: Int = xs
  .map: x => x * x
  .filter: y => y > 0
  (0)

val d2: String = xs
  .map: x => x.toString + xs.dropWhile: y => y > 0
  .filter: z => !z.isEmpty
  (0)

val d3: String = xs
  .map: x => x.toString + xs.collect: case y if y > 0 => y
  .filter: z => !z.isEmpty
  (0)

