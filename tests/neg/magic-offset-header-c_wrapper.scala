//> using options -Ymagic-offset-header:SOURCE_CODE_START_MARKER

val generatedCode = 123

///SOURCE_CODE_START_MARKER:tests/neg/magic-offset-header-c.scala

def userCode =
  val x: String = 0  // anypos-error
