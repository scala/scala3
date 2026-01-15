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
  val ao = Option(1.0).toList

  val x: Unit = use[Int](1, 2, xs*)
  val y = use(1, 2, xs*)
  use(1, xs*, 2)
  use(1, xs*, 2, xs*, 3)
  use(1, xs*, true, ys*, false)
  use(1, identity(xs)*, 2)

  def one = { println("one"); 1 }
  def two = { println("two"); 2 }
  def oneTwoThree = { println("one-two-three"); xs }
  use(one, oneTwoThree*, two)
  //use(1.0, ao*, 2.0)

  val numbers1 = Array(1, 2, 3)
  val numbers2 = List(4, 5, 6)

  def sum(xs: Int*) = xs.sum

  assert(sum(0, numbers1*, numbers2*, 4) == 25)

  // Tests for harmonization with varargs

  val darr: Array[Double] = Array(1.5, 2)
  val zs1 = Array(1, darr*, 2, darr*, 3)
  val _: Array[Double] = zs1
  val d: Double = 4
  val zs2 = Array(1, darr*, 2, d, 3)
  val _: Array[Double] = zs2
  println(zs2.sum)

