//> using options "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s"

def foo(implicit x: Int) = x
val _ = foo(1) // warn