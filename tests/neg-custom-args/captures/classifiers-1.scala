class M extends caps.Mutable

class M1(x: Int => Int) extends M // error

def f(x: M^) = ???

def test(g: Int => Int) = f(new M1(g)) // error


