
def f(using ?): Unit = {} // error: unbound wildcard type
class C(using ?) {}       // error: unbound wildcard type

def f2(using => ?): Unit = {} // error: unbound wildcard type
class C2(using => ?) {}       // error: unbound wildcard type

def f3(using ? *): Unit = {} // error: unbound wildcard type // error
class C3(using ? *) {}       // error: unbound wildcard type // error

def g(using x: ?): Unit = {} // error: unbound wildcard type
class D(using x: ?) {}       // error: unbound wildcard type

val h: (?) ?=> Unit = ??? // ok, but useless (it's a function from Nothing)
