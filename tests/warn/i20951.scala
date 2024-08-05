//> using options  -Wunused:params

// We need the dummy variable: with a constant the unused:params does not trigger a warning but unused:all does.
val dummy = 42
def test(x: Int): Int = dummy // warn

object Foo:
  def f(): Unit = Option(1).map(x => dummy) // warn
  def g(x: Int): Int = dummy // ok
  private def h(x: Int): Int = dummy // warn
  def main(args: Array[String]): Unit = {} // ok

trait Bar:
  def f(x: Int): Int = dummy // ok

abstract class Baz:
  def f(x: Int): Int = dummy // ok

class Qux:
  def f(x: Int): Int = dummy // ok