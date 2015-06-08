class Spec {
  def plus[@specialized T](a: T, b:T)(ev: Numeric[T]): T = plus(b, a)(ev)
}

class IntSpec extends Spec {
  val res = plus(1,2)(Numeric.IntIsIntegral)
}
