import language.experimental.relaxedLambdaSyntax

def fun(f: Int => Int => Int): Int = f(1)(2)

val a = fun: (x: Int) =>
  (y: Int) => x + y

val b = fun: (x: Int) => (y: Int) => x + y

val c = fun: (x: Int) => (y: Int) =>
  x + y

