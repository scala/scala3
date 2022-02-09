def map2(xs: List[Int])(f: Int => Int): List[Int] = xs.map(f)
val f1 = map2
val fc1: List[Int] -> (Int => Int) -> List[Int] = f1

def map3(f: Int => Int)(xs: List[Int]): List[Int] = xs.map(f)
val f2 = map3
val fc2: (Int => Int) -> List[Int] -> List[Int] = f2

val f3 = (f: Int => Int) =>
  println(f(3))
  (xs: List[Int]) => xs.map(_ + 1)
val f3c: (Int => Int) -> {} List[Int] -> List[Int] = f3

class LL[A]:
  def drop(n: Int): {this} LL[A] = ???

def test(ct: CanThrow[Exception]) =
  def xs: {ct} LL[Int] = ???
  val ys = xs.drop(_)
  val ysc: Int -> {ct} LL[Int] = ys



