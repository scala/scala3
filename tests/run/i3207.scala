object Test extends App {
  def foo(ff: String*) = ff
  def bar(bb: String*) = foo(ff = bb: _*)
  println(bar())
  println(bar("A", "B"))
}
