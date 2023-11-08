object A:
  class Box(var value: Int)
  val box: Box = new Box(0)

object B:
  val boxes: Array[A.Box] = Array(A.box)
  val box: A.Box = boxes(0)
  val x: Int = box.value 

// nopos-error: No warnings can be incurred under -Werror.