object Test {

  def compare[S <: T, T] = ()

  compare[Int, Int]
  compare[Int, Any]

  def f[C <: [X] =>> List[X]] = {
    compare[C[Int], List[Int]]
  }


}
