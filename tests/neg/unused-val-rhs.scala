object Test {
  def f(unused i: Int) = {
    val j: Int = i // error
    ()
  }
}
