import annotation.unchecked.uncheckedCaptures

class ArrayBuffer[T]:
  def foreach(op: T => Unit): Unit = ???
def test =
  val tasks = new ArrayBuffer[(() => Unit) @uncheckedCaptures]
  val _: Unit = tasks.foreach(((task: () => Unit) => task()))
