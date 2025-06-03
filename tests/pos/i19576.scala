
object Test:
  val z = Seq(0 -> 1, 2 -> 3).lazyZip(Seq("A", "B"))
  for case ((beg, end), c) <- z yield c // Ok: a withFilter is inserted before map
  for (range, c) <- z yield c           // Ok: exact shape
  for ((beg, end), c) <- z yield c      // Error before changes: Wrong number of parameters, expected 2
