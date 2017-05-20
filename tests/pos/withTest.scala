object Test1 with

  val xs = List(1, 2, 3)

  xs.map with
    x => x + 3

object Test with

  val xs = List(1, 2, 3)

  xs.map with x =>
       x + 2
    .filter with
       x => x % 2 == 0
    .foldLeft(0) with
       _ + _

// another test

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

  try
    val x = 3
  finally
    println("done")

  xs match
    case Nil =>
      println()
      0
    case x :: Nil =>
      1
    case _ => 2

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

    class C extends Object
               with Serializable with

      val x = new C with
          def y = 3

      val result =
        if x == x then
          println("yes")
          true
        else
          println("no")
          false

    // end C
  // end object

