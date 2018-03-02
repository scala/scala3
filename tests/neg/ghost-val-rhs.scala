object Test {
  def f(ghost i: Int) = {
    val j: Int = i // error
    ()
  }
}
