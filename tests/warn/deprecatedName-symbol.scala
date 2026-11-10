//> using options -deprecation

def f(@deprecatedName(Symbol("x")) y: Int): Int = y // warn

val a1 = f(y = 1)

val a2 = f(x = 2) // warn
