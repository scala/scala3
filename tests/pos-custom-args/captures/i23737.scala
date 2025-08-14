import language.experimental.captureChecking

class C

trait A[T]

trait B[CC^] extends A[C^{CC}] // error: CC not found

trait D[CC^]:
  val x: Object^{CC} = ???