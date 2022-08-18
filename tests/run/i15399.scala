@main def Test =
  val tup2Mirror = summon[scala.deriving.Mirror.Of[(Int, Int)]]
  try
    val tup2a: (Int, Int) = tup2Mirror.fromProduct((1, 2, 3)) // fails silently and creates (1, 2)
    println(tup2a)  // should be unreachable
  catch case err: IllegalArgumentException =>
    println("Expected failure when pass Tuple3 to TupleMirror(2):")
    println(s"-  ${err.getMessage}")

  try
    val tup2b: (Int, Int) = tup2Mirror.fromProduct(Tuple(1)) // crashes with index out of bounds
    println(tup2b) // should be unreachable
  catch case err: IllegalArgumentException =>
    println("Expected failure when pass Tuple1 to TupleMirror(2):")
    println(s"-  ${err.getMessage}")
