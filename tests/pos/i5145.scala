class Test {
  def foo(x: Int): Int = {
    val inline = 3
    def opaque(x: Int): Unit = ()
    opaque(3)
    inline
  }
}
