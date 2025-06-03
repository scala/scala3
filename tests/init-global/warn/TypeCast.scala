object A {
  val f: Int = 10
  def m() = f
}
object B {
  val f: Int = g()
  def g(): Int = f // warn
}
object C {
  val a: A.type | B.type = if ??? then A else B
  def cast[T](a: Any): T = a.asInstanceOf[T]
  val c: A.type = cast[A.type](a) // abstraction for c is {A, B}
  val d = c.f // treat as c.asInstanceOf[owner of f].f
  val e = c.m() // treat as c.asInstanceOf[owner of f].m()
  val c2: B.type = cast[B.type](a)
  val g = c2.f // no error here
}

