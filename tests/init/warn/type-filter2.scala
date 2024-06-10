class A(c: C):
  val f: Int = 10
  def m() = f

class B(c: C):
  val f: Int = g()       // warn
  def g(): Int = f

class C(x: Int):
  val a: A | B = if x > 0 then new A(this) else new B(this)

  def cast[T](a: Any): T = a.asInstanceOf[T]

  val c: A = a.asInstanceOf[A]         // abstraction for c is {A, B}
  val d = c.f                          // treat as c.asInstanceOf[owner of f].f
  val e = c.m()                        // treat as c.asInstanceOf[owner of f].m()
  val c2: B = a.asInstanceOf[B]
  val g = c2.f                         // no error here

