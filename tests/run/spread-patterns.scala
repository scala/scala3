import language.experimental.multiSpreads

def use[T](xs: T*) = println(xs)

def useInt(xs: Int*) = ???

@main def Test() =
  val arr: Array[Int] = Array(1, 2, 3, 4, 5, 6)
  val lst = List(1, 2, 3, 4, 5, 6)

  lst match
    case List(1, xs*, 2, 3, 4, 5, 6) =>
      assert(xs.isEmpty)

  lst match
    case List(1, 2, xs*, 6) => println(xs)

  arr match
    case Array(1, 2, xs*, 7) => assert(false)
    case Array(1, 2, 3, xs*) => println(xs)

  arr match
    case Array(xs*, 1, 2)    => assert(false)
    case Array(xs*, 4, 5, 6) => println(xs)

  arr match
    case Array(1, 2, xs*) => println(xs)

  lst match
    case List(1, 2, 3, 4, 5, 6, xs*) => assert(xs.isEmpty)

  lst match
    case Seq(xs*, 1, 2, 3, 4, 5, 6) => assert(xs.isEmpty)





