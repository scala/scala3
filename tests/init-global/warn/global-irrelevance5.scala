object A:
  val array: Array[Int] = new Array(1)
  array(0) = 10

object B:
  var y = A.array(0) * 2 // warn
