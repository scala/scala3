def foo[B](x: B): B = x

def bar[B, A >: B](x: A): A = foo[A](x)

class C[A]

class CV[A](x: Int):
  def this() = this:
    val x = new C[A]:
      println("foo")
    0

