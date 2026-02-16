//> using options -experimental

class Foo():
  @addClass def foo(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> object Baz$macro$1 {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def foo(): Unit =
  //>   Baz$macro$1.run

  @addClass def bar(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> object Baz$macro$2 {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def foo(): Unit =
  //>   Baz$macro$2.run

@main def Test(): Unit =
  new Foo().foo()
  new Foo().bar()
