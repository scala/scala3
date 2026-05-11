def succeed[A](a: A)(trace: Tracer.Trace): A = a
inline def uio[A](a: A): A = succeed(a)(Tracer.autoTrace)

val u = uio(42) // error
