object Test {
  def loadField(x: scala.runtime.IntRef|Null): Unit = x.nn.elem
  def storeField(x: scala.runtime.IntRef|Null): Unit = x.nn.elem = 42

  def check(x: => Any) = try { x; sys.error("failed to throw NPE!") } catch { case _: NullPointerException => }

  def main(args: Array[String]): Unit = {
    check(loadField(null)) // bug: did not NPE under -Ydead-code
    check(storeField(null))

  }
}
