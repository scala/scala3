object A:
  class Box(var value: Int) {
    val initial: Int = value
  }
  val box: Box = new Box(0)

object B:
  val box: A.Box = A.box
  val a: Int = box.initial
  val b: Int = box.value // warn