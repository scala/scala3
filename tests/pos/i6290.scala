//> using options -Werror -deprecation -feature -source:3.3

class TC { type T }

class C(using TC { type T = Int })

def f1(using x: TC) = ???
def f2(using @unchecked x: TC) = ???
inline def f3(using inline x: TC) = ???

class C1(using x: TC)
class C2(using @unchecked x: TC)
class C3(using val x: TC)
class C4(using var x: TC)
class C5(using private val x: TC)
class C6(using private[this] val x: TC)

