object Test {
  def test1(f: [T <: AnyVal] -> List[T] => List[(T, T)]) = {
    f(List(1, 2, 3))
  }

  def main(args: Array[String]): Unit = {
    //test1(...)
  }
}
