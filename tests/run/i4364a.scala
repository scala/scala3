import java.util.function.Consumer

object Test {
  def f(): Unit = assert(false)
  def f(x: Int): Unit = assert(false)
  def f(x: String): Unit = ()

  def foo(c: Consumer[String]) = c.accept("")
  def bar(c: String => Unit) = c("")

  def main(args: Array[String]) = {
    foo(f)
    bar(f)
  }
}
