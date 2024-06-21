//> using options -Werror -explain

def f1: Unit = null // error
def f2: Unit = 1 // error
def f3: Unit = "a" // error
val i: Int = 1
def f4: Unit = i // error
val u: Unit = ()
def f5: Unit = u
