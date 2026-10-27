//> using options -source 3.10

// `_` is still a wildcard before 3.11
def f(xs: List[_]): Int = xs.length
val c = classOf[List[_]]
