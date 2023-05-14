import language.postfixOps
type Tr[+A]
extension [A, B](tr: Tr[A]) inline def map(f: A => B): Tr[B] = ???

extension (d: Double) def func: None.type => Some[Double] = ???

def run[A](query: None.type => Some[A]): Some[A] = ???

val noBug = run(3.14 func) map (x => x)
val buggy = run(3.14 func map (x => x)) // error: missing parameter type