object Test {
  def f(unused i: Int) = {
    def j: Int = i // error
    j
  }
}
