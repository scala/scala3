//> using options -source future

def foo(implicit x: Int) = x
val _ = foo(1) // error
