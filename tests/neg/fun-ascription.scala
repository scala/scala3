def f[T](x: T): T = x
val x1 = (f: Int => Int)  // error
val x2 = f: Int => Int    // error
