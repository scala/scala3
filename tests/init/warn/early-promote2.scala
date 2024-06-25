class M {
  println(this)       // warn
  foo()
  private val a = 5   // warn
  def foo() = a
}
