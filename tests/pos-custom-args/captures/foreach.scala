def test =
  val tasks = new collection.mutable.ArrayBuffer[() => Unit]
  val _: Unit = tasks.foreach(((task: () => Unit) => task()))
