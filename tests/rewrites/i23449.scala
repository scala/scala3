trait T
class C[A]

def f(x: C[_<:T]) = ()

def g(x: C[_>:T]) = ()

def h(x: C[_<: T]) = ()

def k(x: C[_ >: T]) = ()

def m(x: C[_>:Nothing<:T]) = ()

def n(x: C[    _>:Nothing     <:T   ]) = ()
