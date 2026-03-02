trait Listener[+T]

inline def consume[T](f: T => Unit): Listener[T]^{f} = ???

val consumePure = consume(_ => ())