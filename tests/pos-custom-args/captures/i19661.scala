import language.experimental.captureChecking

trait MySet[A]:
  def collect[B](pf: PartialFunction[A, B]^): MySet[B]^{this, pf}

class Test:
  def f(xs: MySet[Int]) = xs collect { case x => x }
  def g(xs: MySet[Int]): MySet[Int] = f(xs)
