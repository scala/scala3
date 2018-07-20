object Test {
  var flag = false

  def f(x: Int): Unit = assert(false)
  def f(x: String): Unit = assert(false)
  def f: java.io.OutputStream = new java.io.OutputStream {
    def write(x: Int) = ()
  }

  def g(x: Int): Unit = flag = true
  def g(x: String): Unit = assert(false)

  def main(args: Array[String]) = {
    val oosF = new java.io.ObjectOutputStream(f)
    oosF.write(0)
    oosF.close()

    val oosG = new java.io.ObjectOutputStream(g) // need warning
    oosG.write(0)
    oosG.close()
    assert(flag)
  }
}
