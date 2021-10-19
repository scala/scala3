def f1[T][U](x: T, y: U): (T, U) = (x, y)
def f2[T](x: T)[U](y: U): (T, U) = (x, y)

@main def test = f2(0)[String]("Hello")