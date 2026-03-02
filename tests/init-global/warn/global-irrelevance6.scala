class Box(x: Int):
  def foo(): Int = 100

object A:
  val array: Array[Box] = new Array(1)
  val n = array(0).foo()       // ok, no crash

object B:
  var y = A.array(0).foo() * 2 // warn
