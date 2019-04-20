
// run with -deprecation -Xfatal-warnings
object Test {
  @deprecated def foo = ???

  implied for Conversion[String, Int] = _.length

  foo    // error

  val x: Int = "abc"
    // OK, since -feature warnings are not enabled.
    // The program compiles with final line
    // there were 1 feature warning(s); re-run with -feature for details
}
