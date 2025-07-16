//> using options -Xmagic-offset-header:///TEST_MARKER
val t1 = 1
val t2 = 2
val t3 = 3
///TEST_MARKER
def test1(): Int = "无穷"  // error
