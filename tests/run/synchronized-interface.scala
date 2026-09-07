// On the JVM this is implemented manually since interface methods can't be synchronized;
// testing in a concurrent context would make this a flaky test, but let's at least ensure
// calling the method works
trait T {
  def meth(x: Int): Int = synchronized {
    if (x < 0) throw new IllegalArgumentException()
    x
  }
}

object O extends T

object Test:
  def main(args: Array[String]): Unit =
    println(O.meth(1))
    try println(O.meth(-1))
    catch case e: IllegalArgumentException => println("ex")

