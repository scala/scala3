import language.experimental.captureChecking

trait IO

type Pair[+T, +U] = [R] -> (op: (T, U) => R) -> R
def cons[T, U](a: T, b: U): Pair[T, U] = [R] => op => op(a, b)
def car[T, U](p: Pair[T, U]): T = p((a, b) => a)
def cdr[T, U](p: Pair[T, U]): U = p((a, b) => b)

def foo(p: Pair[IO^, IO^]): Unit =
  var x: IO^{p*} = null
  x = car[IO^{p*}, IO^{p*}](p)
