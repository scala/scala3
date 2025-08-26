
val x = List(1).map: (x: => Int) => // error
  ???
val z = List(1).map: + => // ok
  ???

val xs = List(1)
val b: Int = xs
  .map: x => x
    * x    // error

val d = xs
  .map: x => x.toString + xs.dropWhile:
    y => y > 0  // error // error

val c = List(xs.map: y => y + y)  // error // error // error // error

val fs: List[List[Int] => Int] = xs.map: x => case y :: ys => y case Nil => -1  // error
