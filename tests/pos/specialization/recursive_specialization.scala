object recursive_specialization {
  class Spec {
    def plus[@specialized T](a: T, b:T)(ev: Numeric[T]): T = plus(b, a)(ev)
  }

  class IntSpec extends Spec {
    lazy val res = plus(1,2)(Numeric.IntIsIntegral)
  }

  def main(args: Array[String]) = {
  }
}
