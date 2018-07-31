import java.io.OutputStream

object Test {
  def main(args: Array[String]): Unit = {
    val oldErr = System.err
    System.setErr(null) // setOut(null) confuses the testing framework...
    val a = () => foo(oldErr)
    a()
  }

  def foo(err: OutputStream): Unit = {
    err.write(0)
  }
}
