class Box(x: Int):
  def foo(): Int = 100

object A:
  val array: Array[Box] = new Array(1)
  array(0) = new Box(10)
  val n = array(0).foo()  // ok

object B:
  var y = A.array(0).foo() * 2 // warn
