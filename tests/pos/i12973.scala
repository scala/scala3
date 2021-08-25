// Compiled and placed on the REPL classpath in the bootstrapCmdTests script
// to test that launching the REPL with `scala -cp <path>` works (issue #12973)
case class Bug12973():
  def check = s"$productPrefix is fixed"
