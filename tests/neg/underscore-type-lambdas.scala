//> using options -source 3.12

// `_` is a type lambda placeholder from 3.12 on, so it can't be used as a wildcard
def f(xs: List[_]): Int = xs.length // error
val c = classOf[List[_]] // error
