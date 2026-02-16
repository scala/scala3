//> using options -Ymagic-offset-header:TEST_MARKER

def x: Int = true  // error

///TEST_MARKER:tests/neg/magic-offset-header-b.scala

def y: Int = false  // anypos-error
