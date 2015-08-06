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
    Tr.foo
  }
}

object Tr {
  def fun(a: Int => Unit) = a(2)
  def foo: Int = {
    var s = 1
    s = try {fun(s = _); 3} catch{ case ex: Throwable => val x = 4; s = x; 5 }
    s
  }
}
