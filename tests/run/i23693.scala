//> using options -Wtostring-interpolated

// verify warning messages and runtime result
// never mind, the test rig doesn't log diagnostics! unlike beloved partest.

case class K(i: Int)

@main def Test =
  val k = K(42)
  println:
    s"k == $k"
  println:
    raw"\k == \$k"
  println:
    f"k == $k"
  println:
    f"k == $k%s"
