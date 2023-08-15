// scalac: -language:experimental.erasedDefinitions

def f(x: => Int, erased y: => Int) = x // error
def g(erased x: => Int, y: => Int) = y // error

val h: (erased => Int, Int) => Int = (erased x, y) => y // error
