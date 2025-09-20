import language.experimental.captureChecking

class C

trait A[T]

trait B[CC^] extends A[C^{CC}] // error: CC not found

trait D[CC^]:
  val x: Object^{CC} = ???

def f(c: C^) =
  val b = new B[{c}] {}
  val a: A[C^{c}] = b