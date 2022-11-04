type C = (() => Int) | (() => String)

def foo(c: C): Unit = ()

val _ = foo(() => 1)
