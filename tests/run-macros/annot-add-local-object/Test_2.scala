//> using options -experimental

@main def Test(): Unit =
  @addClass def foo(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> object Baz {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def foo(): Unit =
  //>   Baz.run

  @addClass def bar(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> object Baz {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def Baz(): Unit =
  //>   Baz.run

  foo()
  bar()
