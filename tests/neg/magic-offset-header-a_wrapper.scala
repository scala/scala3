//> using options -Ymagic-offset-header:TEST_MARKER
val t1 = 1
val t2 = 2
val t3 = 3
///TEST_MARKER:tests/neg/magic-offset-header-a.scala

def test1(): Int = "无穷"  // anypos-error
