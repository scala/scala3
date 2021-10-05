transparent inline def expectCompileError(
  inline code: String,
  expectedMsg: String
) =
  val errors = compiletime.testing.typeCheckErrors(code)
  assert(errors.head.message == expectedMsg, (errors.head.message, expectedMsg))

transparent inline def expectTypeCheck(
  inline code: String,
) : Boolean = compiletime.testing.typeChecks(code)

@main def Test =
  assert(!expectTypeCheck("""compiletime.error("some error")"""))
  assert(expectTypeCheck("""1 + 1"""))
  expectCompileError("""compiletime.error("some error")""", "some error")

