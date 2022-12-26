// scalac: -Wunused:implicits

/* This goes around the "trivial method" detection */
val default_int = 1

def f1(a: Int) = a // OK
def f2(a: Int) = 1 // OK
def f3(a: Int)(using Int) = a // error
def f4(a: Int)(using Int) = default_int // error
def f6(a: Int)(using Int) = summon[Int] // OK
def f7(a: Int)(using Int) = summon[Int] + a // OK

