//> using options -source 3.11 -Xlegacy-wildcards

def f(xs: List[_]): Int = xs.length // warn
val c = classOf[List[_]] // warn
