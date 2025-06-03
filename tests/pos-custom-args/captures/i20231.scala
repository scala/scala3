class Async
class C(val x: Async ?=> Unit)
def foo(x: Async ?=> Unit): C^{x} = C(x)
def foo(x: Async ?=> Unit)(using Async): C^{x} = C(x)