//> using options -experimental -Yno-experimental

import mymacro.addClass

@addClass def foo(): Unit =
  println("macro generated class")
  println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
//> class Baz$macro$1 extends Object {
//>   def run() =
//>     println("macro generated main")
//>     println("executed in: " + getClass.getName)
//> }
//> def foo(): Unit =
//>   new Baz$macro$1.run

@addClass def bar(): Unit =
  println("macro generated class")
  println("executed in: " + (new Throwable().getStackTrace().head.getClassName))
//> class Baz$macro$2 extends Object {
//>   def run() =
//>     println("macro generated main")
//>     println("executed in: " + getClass.getName)
//> }
//> def bar(): Unit =
//>   new Baz$macro$2.run

package a:
  @addClass def foo(): Unit =
    println("macro generated class")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))

  package b:
    @addClass def foo(): Unit =
      println("macro generated class")
      println("executed in: " + (new Throwable().getStackTrace().head.getClassName))

  @addClass def bar(): Unit =
    println("macro generated class")
    println("executed in: " + (new Throwable().getStackTrace().head.getClassName))

  package c:
    @addClass def foo(): Unit =
      println("macro generated class")
      println("executed in: " + (new Throwable().getStackTrace().head.getClassName))

@main def Test(): Unit =
  foo()
  bar()
  a.foo()
  a.b.foo()
  a.bar()
  a.c.foo()
