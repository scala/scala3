class A(x: Int) {
  def foo(): Int = {
    val to = x
    var sum = 0
    var i = 0
    while i < to do
      sum += i
      i += 1

    B.a + 10 + sum
  }
}

object B {
  val a: Int = A(4).foo()
}

