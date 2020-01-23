class TC { type T }

class C with (TC { type T = Int })

def f1 with (x: TC) = ???
def f2 with (@unchecked x: TC) = ???
inline def f3 with (inline x: TC) = ???

class C1 with (x: TC)
class C2 with (@unchecked x: TC)
class C3 with (val x: TC)
class C4 with (var x: TC)
class C5 with (private val x: TC)
class C6 with (private[this] val x: TC)

