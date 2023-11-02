//> using options -Werror -explain

def f1: Unit = null // warn
def f2: Unit = 1 // warn
def f3: Unit = "a" // warn
val i: Int = 1
def f4: Unit = i // warn
val u: Unit = ()
def f5: Unit = u
// nopos-error: No warnings can be incurred under -Werror.