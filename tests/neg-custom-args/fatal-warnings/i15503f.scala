// scalac: -Wunused:implicits

def f1(a: Int) = a // OK
def f2(a: Int) = 1 // OK
def f3(a: Int)(using Int) = a // error
def f4(a: Int)(using Int) = 1 // error
def f6(a: Int)(using Int) = summon[Int] // OK
def f7(a: Int)(using Int) = summon[Int] + a // OK

