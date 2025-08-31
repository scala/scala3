import language.experimental.multiSpreads

def use[T](xs: T*) = println(xs)

def useInt(xs: Int*) = ???

@main def Test() =
  val arr: Array[Int] = Array(1, 2, 3)
  use(arr*)

  val iarr: IArray[Int] = IArray(1, 2, 3)
  use(iarr*)

  val xs = List(1, 2, 3)
  val ys = List("A")

  val x: Unit = use[Int](1, 2, xs*)
  val y = use(1, 2, xs*)
  use(1, xs*, 2)
  use(1, xs*, 2, xs*, 3)
  use(1, xs*, true, ys*, false)
