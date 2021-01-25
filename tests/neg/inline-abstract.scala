class A:
  inline def f(): Int

class B extends A:
  inline def f() = 1

def Test =
  val b = B()
  println(b.f())  // ok
  val a: A = b
  println(a.f())  // error: Deferred inline method f in class A cannot be invoked
