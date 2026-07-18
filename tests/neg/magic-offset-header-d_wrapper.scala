//> using options -Ymagic-offset-header:SOURCE_CODE_START_MARKER

def test1: String = 0 // error
///SOURCE_CODE_START_MARKER:something_nonexist.scala
def test2: Int = "0" // error
