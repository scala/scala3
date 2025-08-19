class A extends caps.SharedCapability:
  def even(x: Int) = x % 2 == 0

def f(a: A): List[Int] ->{a} List[Int] =
  (xs: List[Int]) => xs.collect:
    case x if a.even(x) => 1

