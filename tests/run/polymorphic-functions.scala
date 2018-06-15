object Test {
  def test1(f: [T <: AnyVal] -> List[T] => List[(T, T)]) = {
    f(List(1, 2, 3))
  }

  def main(args: Array[String]): Unit = {
    val fun = new PolyFunction {
      def apply[T <: AnyVal](x: List[T]): List[(T, T)] = x.map(e => (e, e))
    }

    assert(test1(fun) == List((1, 1), (2, 2), (3, 3)))
  }
}
