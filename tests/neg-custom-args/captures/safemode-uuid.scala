import language.experimental.safe

object Test:
  val ok = new java.util.UUID(1, 2)
  val bad = java.util.UUID.randomUUID() // error: randomUUID must not be usable from safe code
