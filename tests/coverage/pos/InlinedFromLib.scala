package covtest

// assert is a `transparent inline` in Predef,
// but its source path should not appear in the coverage report.
def testInlined(): Unit =
  val l = 1
  assert(l == 1)
  assert(l == List(l).length)
  assert(List(l).length == 1)
