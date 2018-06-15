object Test {
  def test1(f: PolyFunction { def apply[T <: AnyVal](x: List[T]): List[(T, T)] }) = {
    f(List(1, 2, 3))
  }

  def main(args: Array[String]): Unit = {
    //test1(...)
  }
}
