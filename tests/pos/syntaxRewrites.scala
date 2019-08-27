// Compile with -rewrite -pascal-style
// Then compile again with -rewrite c-style
// The resulting file is the same as the original one, except for some extra spaces
// at line ends
object Test {

  val xs = List(1, 2, 3)

  for (x <- xs) yield x * 2

  for (x <- xs)
  yield x * 2

  for { x <- xs; y <- xs } yield x * y

  for {
    x <- xs
    y <- xs
  } yield x * y

  for {
    x <- xs
    y <- xs
  } yield x * y

  for { x <- xs }
  yield x * 2

// -----------------------------------------------

  for (x <- xs) println(x)

  for (x <- xs)
    println(x)

  for { x <- xs; y <- xs } println(x * y)

  for {
    x <- xs
    y <- xs
  }
  println(x * y)

  for {
    x <- xs
    y <- xs
  } println(x * y)

  for { x <- xs }
    println(x)

  if (xs == Nil) println("yes")

  if (xs == Nil)
    println("yes")

  if (xs == Nil
     && xs.length == 0)
    println("yes")

  while (xs == Nil) println("yes")

  while ({
    val ys = xs ++ xs
    ys.nonEmpty
  }) println("yes")

  while ({
    val ys = xs ++ xs
    ys.nonEmpty
  })
  println("yes")
}