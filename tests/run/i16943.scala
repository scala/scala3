@main
@annotation.experimental
def Test(): Unit = fail(caps.unsafe.unsafeErasedValue, 1)

@annotation.experimental
def fail(dumb: CanThrow[Exception], x: Int) = println(x)
