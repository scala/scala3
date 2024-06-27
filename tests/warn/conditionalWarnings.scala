//> using options -deprecation 

object Test {
  @deprecated def foo = ???

  given Conversion[String, Int] = _.length

  foo    // warn

  val x: Int = "abc"
    // OK, since -feature warnings are not enabled.
    // The program compiles with final line
    // there was 1 feature warning; re-run with -feature for details
    // nopos-warn
}