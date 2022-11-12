class Foo {
  def member: String = this.toString
}
def method[T](c: T => Boolean): T = ???
val y: Foo => Boolean = _ => true
val x : String = "5"
val _ = method(y).member // error