
def f1[T]()[U](x: T, y: U): (T, U) = (x, y)
def f2[T](x: T)[U](y: U): (T, U) = (x, y)
def f3[T, U](using DummyImplicit)[V](x: T): U = ???
def f4[T](x: T)[U <: x.type](y: U): (T, U) = (x, y)
