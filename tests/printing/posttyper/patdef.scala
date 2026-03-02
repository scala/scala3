val (a) = (1)
val (b, c) = (2, 3)
val (d, _, e) = (4, 5, 6)

val (f): (Int) = (7)
val (g, h): (Int, Int) = (8, 9)
val (i, _, j): (Int, Int, Int) = (10, 11, 12)

def foo(): (Int, Int) = ???

val (k, l) = foo()
val (m, _) = foo()
val (n, o): (Int, Int) = foo()
val (p, _): (Int, Int) = foo()

val ((q, r)) = ((13, 14))

def cond(): Boolean = ???

val (s, t) = if cond() then (15, 16) else (17, 18)
