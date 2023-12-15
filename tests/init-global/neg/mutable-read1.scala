class Box(var value: Int)

object A:
  val box: Box = new Box(4)

object B:
  val boxB: Box = new Box(5)
  val boxA: Box = A.box
  val m: Int = boxB.value
  val n: Int = boxA.value 
// nopos-error: No warnings can be incurred under -Werror.