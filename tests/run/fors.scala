//############################################################################
// for-comprehensions (old and new syntax)
//############################################################################

//############################################################################

object Test extends App {
  val xs = List(1, 2, 3)
  val ys = List(Symbol("a"), Symbol("b"), Symbol("c"))

  def it = 0 until 10

  val ar = "abc".toCharArray

  /////////////////// old syntax ///////////////////

  def testOld(): Unit = {
    println("\ntestOld")

    // lists
    for (x <- xs) print(x + " "); println()
    for (x <- xs;
         if x % 2 == 0) print(x + " "); println()
    for {x <- xs
         if x % 2 == 0} print(x + " "); println()
    var n = 0
    for (_ <- xs) n += 1; println(n)
    for ((x, y) <- xs zip ys) print(x + " "); println()
    for (p @ (x, y) <- xs zip ys) print(p._1 + " "); println()

    // iterators
    for (x <- it) print(x + " "); println()
    for (x <- it;
         if x % 2 == 0) print(x + " "); println()
    for {x <- it
         if x % 2 == 0} print(x + " "); println()

    // arrays
    for (x <- ar) print(x + " "); println()
    for (x <- ar;
         if x.toInt > 97) print(x + " "); println()
    for {x <- ar
         if x.toInt > 97} print(x + " "); println()

  }

  /////////////////// new syntax ///////////////////

  def testNew(): Unit = {
    println("\ntestNew")

    // lists
    var n = 0
    for (_ <- xs) n += 1; println(n)
    for ((x, y) <- xs zip ys) print(x + " "); println()
    for (p @ (x, y) <- xs zip ys) print(p._1 + " "); println()

    // iterators
    for (x <- it) print(x + " "); println()
    for (x <- it if x % 2 == 0) print(x + " "); println()
    for (x <- it; if x % 2 == 0) print(x + " "); println()
    for (x <- it;
         if x % 2 == 0) print(x + " "); println()
    for (x <- it
         if x % 2 == 0) print(x + " "); println()
    for {x <- it
         if x % 2 == 0} print(x + " "); println()
    for (x <- it;
         y = 2
         if x % y == 0) print(x + " "); println()
    for {x <- it
         y = 2
         if x % y == 0} print(x + " "); println()

    // arrays
    for (x <- ar) print(x + " "); println()
  }

  /////////////////// filtering with case ///////////////////

  def testFiltering(): Unit = {
    println("\ntestFiltering")

    val xs: List[Any] = List((1, 2), "hello", (3, 4), "", "world")

    for (case x: String <- xs) do print(s"$x "); println()
    for (case (x: String) <- xs) do print(s"$x "); println()
    for (case y@ (x: String) <- xs) do print(s"$y "); println()

    for (case (x, y) <- xs) do print(s"$x~$y "); println()

    for (case (x: String) <- xs if x.isEmpty) do print("(empty)"); println()
    for (case (x: String) <- xs; y = x) do print(s"$y "); println()
    for (case (x: String) <- xs; case (y, z) <- xs) do print(s"$x/$y~$z "); println()

    for (case (x, y) <- xs) do print(s"${(y, x)} "); println()

    for case x: String <- xs do print(s"$x "); println()
    for case (x: String) <- xs do print(s"$x "); println()
    for case y@ (x: String) <- xs do print(s"$y "); println()

    for case (x, y) <- xs do print(s"$x~$y "); println()

    for case (x: String) <- xs if x.isEmpty do print("(empty)"); println()
    for case (x: String) <- xs; y = x do print(s"$y "); println()
    for case (x: String) <- xs; case (y, z) <- xs do print(s"$x/$y~$z "); println()

    for case (x, y) <- xs do print(s"${(y, x)} "); println()
  }

  def testGivens(): Unit = {
    println("\ntestGivens")

    // bound given that is summoned in subsequent bind
    for
      a <- List(123)
      given Int = a
      b = summon[Int]
    do
      println(b)

    // generated given that is summoned in subsequent bind
    for
      given Int <- List(456)
      x = summon[Int]
    do
      println(x)

    // pick the correct given
    for
      a <- List(789)
      given Int = a
      given Int <- List(0)
      x = summon[Int]
    do
      println(x)
  }

  ////////////////////////////////////////////////////

  testOld()
  testNew()
  testFiltering()
  testGivens()
}
