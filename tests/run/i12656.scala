transparent inline def expectCompileError(
  inline code: String,
  expectedMsg: String
) =
  val errors = compiletime.testing.typeCheckErrors(code)
  assert(errors.head.message == expectedMsg, (errors.head.message, expectedMsg))

@main def Test = expectCompileError("""compiletime.error("some error")""", "some error")