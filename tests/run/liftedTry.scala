object Test {

  def raise(x: Int) = { throw new Exception(s"$x"); 0 }
  def handle: Throwable => Int = { case ex: Exception => ex.getMessage().toInt }

  val x = try raise(1) catch handle

  def foo(x: Int) = {
    val y = try raise(x) catch handle
    y
  }

  foo(try 3 catch handle)

  def main(args: Array[String]) = {
    assert(x == 1)
    assert(foo(2) == 2)
    assert(foo(try raise(3) catch handle) == 3)
    assert(Tr.foo == 3)
  }
}

object Tr {
  def fun(a: Int => Unit) = a(2)
  def foo: Int = {
    var s = 1
    s = try {fun(s = _); 3} catch { case ex: Throwable => val x = 4; s = x; 5 }
    s
  }
}

/* was:
Caused by: java.lang.VerifyError: Inconsistent stackmap frames at branch target 33
Exception Details:
  Location:
    Tr$.foo()I @30: goto
  Reason:
    Current frame's stack size doesn't match stackmap.
  Current Frame:
    bci: @30
    flags: { }
    locals: { 'Tr$', 'scala/runtime/IntRef', 'java/lang/Throwable', integer }
    stack: { integer }
  Stackmap Frame:
    bci: @33
    flags: { }
    locals: { 'Tr$', 'scala/runtime/IntRef' }
    stack: { top, integer }
 */
