val f = [R] => () => [A] => (a: A => R, b: A) => a(b)
val x = f[Int]()(_, 3)
