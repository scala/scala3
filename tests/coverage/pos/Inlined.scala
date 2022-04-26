package covtest

// Checks that we use the new positions of the inlined code properly
def testInlined(): Unit =
  val l = 1
  assert(l == 1) // scala.Predef.assert is inline in dotty
  assert(l == List(l).length)
  assert(List(l).length == 1)
