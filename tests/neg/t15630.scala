
def f = summon[Int] // error

trait T

def g(using T) = 42

def h: Int = g // error
