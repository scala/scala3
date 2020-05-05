trait MyRange {
  inline def foreach(inline f: Int => Unit): Unit
}

transparent inline def MyRange(inline start: Int, inline end: Int): MyRange = new {
  inline def foreach(inline f: Int => Unit) = // error: Implementation restriction
    var i = start
    val e = end
    while (i < e) {
      f(i)
      i += 1
    }
}

object App {
  val count: Int = 4
  for (i <- MyRange(0, count)) { // error: Deferred inline method foreach in trait MyRange cannot be invoked
    Console.println("Number: " + i)
  }
}
