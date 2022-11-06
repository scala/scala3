// scalac: -Wunused:explicits

def f1(a: Int) = a // OK
def f2(a: Int) = 1 // error
def f3(a: Int)(using Int) = a // OK
def f4(a: Int)(using Int) = 1 // error
def f6(a: Int)(using Int) = summon[Int] // error
def f7(a: Int)(using Int) = summon[Int] + a // OK