object O {

  class CA[A](var x: A)
  type C = CA[_]

  val c: C = ???
  def f[A](r: CA[A]): A = r.x

  def g(): CA[_] = CA("hello")

  f(g())
  f(c)
  f(c.asInstanceOf[CA[_]])
  f(c.asInstanceOf[C])
  f(c.asInstanceOf[c.type])
}
