// This test checks that the example given in the `-explain` of the
// `DoubleDefinition` message is correct.

def f[T](x: T): Unit = ???
def f(x: Any): Unit = ??? // error
