val x: Function1[Int, Int] { def apply(arg: Int): Int } = x => x
val x1 = x
val y = x.apply(arg = 1)
