//> using options -Xcheck-macros

object Main:
  val f: String => String =
    Macros.foo // error - the closure does not refer to an anonymous function

  @main def go =
    println(f("Hello"))
