class BreakControl extends Throwable

object Breaks:
  private val breakException = new BreakControl

  def breakable(op: => Unit): Unit =
    try op catch { case ex: BreakControl if ex eq breakException => }

  def break(): Nothing = throw breakException

object A:
  val n = foo("hello")
  def foo(s: String): Int =
    val len = s.length
    var i = 0

    while (i < len) {
      Breaks.breakable {
        val c = s.charAt(i)

        if c == '\n' then Breaks.break()
      }
      i += 1
    }

    i
