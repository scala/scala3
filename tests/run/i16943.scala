@main
@annotation.experimental
def Test(): Unit = fail(compiletime.erasedValue, 1)

@annotation.experimental
def fail(dumb: CanThrow[Exception], x: Int) = println(x)
