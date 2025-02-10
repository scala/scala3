//> using options -source 3.7

def foo(implicit x: Int) = x
val _ = foo(1) // warn
