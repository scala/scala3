//> using options -source 3.7-migration

def foo(implicit x: Int) = ()
val _ = foo(1)
val _ = foo    (1)