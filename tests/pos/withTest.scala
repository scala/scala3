object Test with

   val xs = List(1, 2, 3)

// Plain indentation

   xs.map with
        x => x + 2
     .filter with
        x => x % 2 == 0
     .foldLeft(0) with
        _ + _

// Using lambdas with `with`

   xs.map with x =>
        x + 2
     .filter with x =>
        x % 2 == 0
     .foldLeft(0) with
        _ + _

// for expressions

   for
      x <- List(1, 2, 3)
      y <- List(x + 1)
   yield
      x + y

   for
      x <- List(1, 2, 3)
      y <- List(x + 1)
   do
      println(x + y)


// Try expressions

   try
      val x = 3
      x
   catch
      case ex: Exception =>
         0
   finally
      println("done")

// Match expressions

   xs match
      case Nil =>
         println()
         0
      case x :: Nil =>
         1
      case _ => 2

// While and Do

   do
      println("x")
      println("y")
   while
      println("z")
      true

   while
      println("z")
      true
   do
      println("x")
      println("y")

   // end while

// end Test

package p with

  object o with

    class B with self =>
      def f(x: Int) = ???

    val x = 3

    enum Color with
      case Red, Green, Blue

    class C extends Object with Serializable with self =>
      val result =
        if x == x then
          println("yes")
          true
        else
          println("no")
          false

    // end C
  // end o
