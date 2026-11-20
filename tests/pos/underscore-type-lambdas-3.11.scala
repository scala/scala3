//> using options -source 3.11

// `_` is still a wildcard before 3.12
def f(xs: List[_]): Int = xs.length
val c = classOf[List[_]]
