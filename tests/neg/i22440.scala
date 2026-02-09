//> using options -source future

def foo(implicit x: Int) = x // error
val _ = foo(1) // error
