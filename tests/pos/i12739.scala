object X {

  class CA[A]
  type C = CA[_]
  val c: C = ???
  def f[A](r: CA[A]) = ()
  def g(): CA[_] = CA()
  def h(): C = ???

  // works
  f(c)

  // works
  val x = c.asInstanceOf[C]
  f(x)

  // was: error
  f(c.asInstanceOf[C])

  // works, error in Scala 2
  f(c.asInstanceOf[c.type])

  f(c.asInstanceOf[CA[_]])
  f(g())
  f(h())
}
