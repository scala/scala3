//> using options -Wtostring-interpolated

// verify ~warning messages and~ runtime result
// never mind, the test rig doesn't log diagnostics! unlike beloved partest.

// Sadly, junit is not available.
//import org.junit.Assert.assertEquals as jassert

def assertEquals(expected: String)(actual: String): Unit = assert(expected == actual)

case class K(i: Int)

@main def Test =
  val k = K(42)
  assertEquals("k == K(42)"):
    s"k == $k"
  assertEquals("\\k == \\K(42)"):
    raw"\k == \$k"
  assertEquals("k == K(42)"):
    f"k == $k"
  assertEquals("k == K(42)"):
    f"k == $k%s"
