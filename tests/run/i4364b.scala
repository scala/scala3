object Test {
  def f(x: Int): Unit = assert(false)
  def f(x: String): Unit = assert(false)
  def f: java.io.OutputStream = new java.io.OutputStream {
    def write(x: Int) = ()
  }

  def main(args: Array[String]) = {
    val oos = new java.io.ObjectOutputStream(f)
    oos.write(0)
    oos.close()
  }
}
