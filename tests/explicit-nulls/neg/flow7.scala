
class Foo(x: String|Null) {
  if (x == null) throw new NullPointerException("x is null")
  val y: String = x // error: flow inference for blocks only works inside methods

  def foo(x: String|Null): Unit = {
    if (x == null) throw new NullPointerException("x is null")
    val y: String = x
    ()
  }
}
