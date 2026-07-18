package covtest

// $COVERAGE-OFF$
class WholeFileIgnored:
  def f(x: Int): Int = x + 1

  def g: String = "hello"

  def h(x: Int): String =
    if x > 0 then "positive"
    else "negative"

object WholeFileIgnoredObj:
  val x = 42
  def compute(a: Int, b: Int): Int = a + b
