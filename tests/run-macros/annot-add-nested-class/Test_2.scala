//> using options -experimental

class Foo():
  @addClass def foo(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> class Baz$macro$1 extends Object {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def foo(): Unit =
  //>   new Baz$macro$1.run

  @addClass def bar(): Unit =
    println("macro generated main")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
  //> class Baz$macro$2 extends Object {
  //>   def run() =
  //>     println("macro generated main")
  //>     println("executed in: " + getClass.getName)
  //> }
  //> def foo(): Unit =
  //>   new Baz$macro$2.run

@main def Test(): Unit =
  new Foo().foo()
  new Foo().bar()
