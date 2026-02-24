trait FileSystem:
  def read(): Unit
def process(fs: FileSystem^): Unit =
  val f: () -> Unit = () => fs.read()  // Error: fs cannot flow into {}