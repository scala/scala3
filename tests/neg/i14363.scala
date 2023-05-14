def f[T <: Ordered[T]](t: T): T = t
def test = f(1)  // error