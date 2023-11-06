def foo[sealed B](x: B): B = x

def bar[B, sealed A >: B](x: A): A = foo[A](x)

class C[sealed A]

class CV[sealed A](x: Int):
  def this() = this:
    val x = new C[A]:
      println("foo")
    0

