type ZZ = String ?=> Int
def f(xs: ZZ*) = xs.zipWithIndex.foreach((f: ZZ, i) => f(using "s"))
