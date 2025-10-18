class X {
  def bar(): Int = 5
}
class Y extends X {
  override def bar(): Int = 6
}

object O {
  def foo(p: => X) = {
    p.bar()
  }

  def foo2(q: => X) = foo(q)
  def foo3(r: => X) = foo(r)

  val a = foo(new X)
  val b = foo(new Y)
  val c = foo2(new Y)
  val d = foo3(new Y)
}

/**
  * Pass arg to by-name parameter: create a Fun where body is the argument expression
  * Read value of by-name parameter: call 'apply' on every possible Fun value of the by-name parameter
  * Solution: Add special EnvRefs for by-name params;
  *           differentiate these EnvRefs by the arg tree passed to the by-name param
  */