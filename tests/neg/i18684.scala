//> using option -explain
object Test:
  val s(): String = "hello, world" // error

  val i() = 22 // error

  def foo(): String = "22"

  object inner:
    val foo() = "33" // error

  val inner(x) = 3 // error