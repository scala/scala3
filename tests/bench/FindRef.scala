class MyInt(val x: Int) {
  def eq(that: MyInt): Boolean = this.x == that.x
}

class Test {
  def foo(x: MyInt, y: MyInt): Boolean = x.eq(y)

  val a = MyInt(2)
  val b = MyInt(3)
  foo(a, b)
}