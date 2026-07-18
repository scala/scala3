import caps.*

def Test =
  val c: Object^ = "a"
  val d: Object^ = "a"
  val e1, e2: Object^ = "a"
  val falseDeps: Object^ = "a"
  def f[T](consume x: T): Unit = ()
  def g(consume x: Object^): Unit = ()
  def h(consume x: (Object^, Object^)): Object^ = x._1  // error: local reach leak
  val cc = (c, c)
  f(cc)  // no consume, it's boxed
  f(c)   // no consume, it's boxed
  println(c)  // ok
  g(d)   // consume
  println(d) // error
  h((e1, e2))  // no consume, still boxed
  println(e1)  // ok

  class Ref extends Mutable
  def i[T <: Ref](consume x: Ref): Ref = x





