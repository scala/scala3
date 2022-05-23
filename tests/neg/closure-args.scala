import language.experimental.fewerBraces

val x = List().map: (x: => Int) => // error
  ???
val y = List() map: x =>  // error
  x + 1   // error
val z = List().map: + => // ok
  ???

val xs = List(1)
val d = xs
  .map: x => x.toString + xs.dropWhile:
    y => y > 0  // error

