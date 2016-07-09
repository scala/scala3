object Test {

  def compare[S <: T, T] = ()

  compare[Int, Int]
  compare[Int, Any]

  def f[C <: List] = {
    compare[C[Int], List[Int]]
  }


}
