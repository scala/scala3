//> using options -Xcheck-macros

object Main:
  val f: String => String =
    Macros.foo

  @main def go =
    println(f("Hello"))
