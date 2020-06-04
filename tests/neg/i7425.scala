class C { def f: Int = 0 }

trait D { def f(): Int = 0 }

def foo1(x: C & D) = x.f // ok
def foo2(x: C | D) = x.f // error: value f is not a member of C | D
def foo3(x: D & C) = x.f // ok
def foo4(x: D | C) = x.f // error: value f is not a member of D | C
def foo5(x: C & D) = x.f() // ok
def foo6(x: C | D) = x.f() // error: value f is not a member of C | D
def foo7(x: D & C) = x.f() // ok
def foo8(x: D | C) = x.f() // error: value f is not a member of D | C
