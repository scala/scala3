class Test {
  def foo(x: Int): Int = {
    val inline = 3
    def opaque(x: Int): Unit = ()
    opaque(3)
    inline
  }
  def bar(inline: Int => Int) = 3
  inline def baz(inline x: Int => Int) = 3

  locally {
    bar(inline = identity)
    bar(inline => inline)
    bar(implicit inline => inline)
  }
}
