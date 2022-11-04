package covtest

// Checks that we use the new positions of the inlined code properly
def testInlined(): Unit =
  val l = 1
  assert(l == 1)
  assert(l == List(l).length)
  assert(List(l).length == 1)

transparent inline def assert(inline assertion: Boolean): Unit =
  if !assertion then scala.runtime.Scala3RunTime.assertFailed()
