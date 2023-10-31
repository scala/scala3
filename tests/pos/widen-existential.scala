class A {
  { val x = classOf[List[?]] }
  def f = {
    val g = classOf[List[?]]
    List(g, g)
  }
}
