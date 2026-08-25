//> using options -Wunused:all

trait Parent:
  def advanceSpanRange(spanFtIdx: Int): Int = spanFtIdx + 1

object Reported extends Parent:
  private var spanFtIdx: Int = 0 // nowarn, read as argument of advanceSpanRange
  def advanceSpanTo(): Unit =
    spanFtIdx = super.advanceSpanRange(spanFtIdx)

object ArgRead:
  private var i: Int = 0 // nowarn, read as argument of f
  def f(x: Int): Int = x + 1
  def go(): Unit = i = f(i)

object ReceiverRead:
  private var b = new StringBuilder // nowarn, read as receiver of append
  def go(): Unit = b = b.append("x")

object PureCycle:
  private var i: Int = 0 // warn, value only cycles through pure arithmetic
  def go(): Unit = i = i * 2 + 1

object PureCycleOther:
  private var i: Int = 0 // warn, value only cycles through pure arithmetic
  private var j: Int = 1
  def go(): Unit =
    i = j + i
    j = 2

object SelfIncr:
  private var i: Int = 0 // warn, mutated but not read
  def go(): Unit = i += 1

object ClosureEscape:
  private var i: Int = 0 // nowarn, read by the closure assigned to f
  private var f: () => Int = () => 0
  def go(): Unit =
    i = { f = () => i; 0 }
  def read(): Int = f()

object LocalDefEscape:
  private var i: Int = 0 // nowarn, read via h, which is passed to sink
  def sink(x: Int): Unit = ()
  def go(): Unit =
    i =
      def h = i
      sink(h)
      0

object DifferentVarInRhs extends Parent:
  private var spanFtIdx: Int = 0 // warn, mutated but never read
  private var otherVar: Int = 0 // nowarn, mutated and read as argument
  def advanceSpanTo(): Unit =
    spanFtIdx = super.advanceSpanRange(otherVar)
    otherVar = 1
