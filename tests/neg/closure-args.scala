import language.experimental.fewerBraces

val x = List(1).map: (x: => Int) => // error
  ???
val z = List(1).map: + => // ok
  ???

val xs = List(1)
val b: Int = xs       // error
  .map: x => x * x    // error
  .filter: y => y > 0  // error
  (0)
val d = xs   // error
  .map: x => x.toString + xs.dropWhile:
    y => y > 0

val c = List(xs.map: y => y + y)  // error // error // error // error
val d2: String = xs    // error
  .map: x => x.toString + xs.dropWhile: y => y > 0  // error // error
  .filter: z => !z.isEmpty // error
  (0)

val fs: List[List[Int] => Int] = xs.map: x => case y :: ys => y case Nil => -1  // error // error
