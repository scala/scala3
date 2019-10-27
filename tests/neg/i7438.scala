type Tr[+A]
inline def (tr: Tr[A]) map[A, B](f: A => B): Tr[B] = ???

def (d: Double) func: None.type => Some[Double] = ???

def run[A](query: None.type => Some[A]): Some[A] = ???

val noBug = run(3.14 func) map (x => x)
val buggy = run(3.14 func map (x => x)) // error: missing parameter type