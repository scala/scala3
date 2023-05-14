
// run with -deprecation -Xfatal-warnings
object Test {
  @deprecated def foo = ???

  given Conversion[String, Int] = _.length

  foo    // error

  val x: Int = "abc"
    // OK, since -feature warnings are not enabled.
    // The program compiles with final line
    // there was 1 feature warning; re-run with -feature for details
    // nopos-error
}
