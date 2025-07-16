//> using options -Xmagic-offset-header:///TEST_MARKER

def x: Int = true  // error

///TEST_MARKER

def y: Int = false  // error
