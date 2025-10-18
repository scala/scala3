package covtest

// Checks that we use the new positions of the inlined code properly
// NOTE (12.08.2025): After recent changes, the inlined nodes will not be tagged in coverage
def testInlined(): Unit =
  val l = 1
  assert(l == 1)
  assert(l == List(l).length)
  assert(List(l).length == 1)

transparent inline def assert(inline assertion: Boolean): Unit =
  if !assertion then scala.runtime.Scala3RunTime.assertFailed()
