import language.experimental.multiSpreads

def use[T](xs: T*) = println(xs)

def useInt(xs: Int*) = ???

@main def Test() =
  val arr: Array[Int] = Array(1, 2, 3, 4, 5, 6)
  val xs = List(1, 2, 3, 4, 5, 6)

  xs match
    case List(1, 2, xs*, ys*, 6) => println(xs) // error



