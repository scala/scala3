object X {
  val x: Int = null.asInstanceOf[Nothing]
  def d: Int = null.asInstanceOf[Nothing]
  var s: Int = 0
  s = null.asInstanceOf[Nothing]
  def takeInt(i: Int): Unit
  takeInt(null.asInstanceOf[Nothing])
}

object Y {
  val n: Null = null
  val x: Object = n
  def d: Object = n
  var s: Object = 0
  s = n
  def takeInt(i: Object): Unit
  takeInt(n)
}
