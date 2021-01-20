class A
trait B
trait C

object Test extends A with
  B  with  // error // error this one allows an informative hint
  C

object Test2 extends A with
  B { // error  this one is harder since it is syntactically correct
    println("foo")
  }

def foo: A with
  B with
  C = ???




