object Test {
  def f(ghost i: Int) = {
    def j: Int = i // error
    j
  }
}
