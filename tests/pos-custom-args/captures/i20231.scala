import caps.cap

class Async
class C(val x: Async ?=> Unit)
def foo(x: Async ?=> Unit): C^{x} = C(x)
def foo(x: Async ?=> Unit)(using Async): C^{x} = C(x)

// should also work for reach and read-only capabilities
class D(val x: Async ?=> Unit) extends caps.Mutable
def bar1(x: Async ?=> Unit): D^{cap, x*} = D(x)
def bar1(x: Async ?=> Unit)(using Async): D^{cap, x*} = D(x)
def bar2(x: Async ?=> Unit): D^{cap.rd, x.rd} = D(x)
def bar2(x: Async ?=> Unit)(using Async): D^{cap.rd, x.rd} = D(x)

