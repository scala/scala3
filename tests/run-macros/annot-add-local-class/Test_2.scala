//> using options -experimental -Yno-experimental

@main def Test(): Unit =
  @addClass def foo(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> class Baz extends Object {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def foo(): Unit =
  //>   new Baz().run

  @addClass def bar(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> class Baz extends Object {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def Baz(): Unit =
  //>   new Baz().run

  foo()
  bar()
