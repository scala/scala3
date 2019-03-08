import java.util.function.Consumer

object Test {
  def f(x: String): Unit = assert(false)
  def f: Consumer[String] = new Consumer { def accept(s: String) = () }

  def foo(c: Consumer[String]) = c.accept("")

  def main(args: Array[String]) = {
    foo(f)
  }
}
