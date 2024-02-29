import annotation.unchecked.uncheckedCaptures
def test =
  val tasks = new collection.mutable.ArrayBuffer[(() => Unit) @uncheckedCaptures]
  val _: Unit = tasks.foreach(((task: () => Unit) => task()))
