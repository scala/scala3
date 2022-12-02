val f = [A] => (a: A) => a
val x1 = f.apply[Int](4)
val x2 = f.apply[4](4)
