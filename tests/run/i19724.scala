class F0 extends (() => Double):
  inline def apply(): Double = 1.toDouble

class F1 extends (Int => Double):
  inline def apply(v: Int): Double = v.toDouble

class F2 extends ((Int, Int) => Double):
  inline def apply(v1: Int, v2: Int): Double = (v1 + v2).toDouble

@main def Test =
  val f0: (() => Double) = new F0
  assert(f0() == 1.0)

  val f1: (Int => Double) = new F1
  assert(f1(3) == 3.0)

  val f2: ((Int, Int) => Double) = new F2
  assert(f2(3, 2) == 5.0)
