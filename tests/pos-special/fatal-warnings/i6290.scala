class TC { type T }

class C given (TC { type T = Int })

def f1 given (x: TC) = ???
def f2 given (@unchecked x: TC) = ???
inline def f3 given (inline x: TC) = ???

class C1 given (x: TC)
class C2 given (@unchecked x: TC)
class C3 given (val x: TC)
class C4 given (var x: TC)
class C5 given (private val x: TC)
class C6 given (private[this] val x: TC)

