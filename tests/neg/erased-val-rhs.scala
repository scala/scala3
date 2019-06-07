object Test {
  def f erased (i: Int) = {
    val j: Int = i // error
    ()
  }
}
