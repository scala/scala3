trait X:
  type T
  def process(t: T): Unit

abstract class Z:
  def x1: X
  val x: X = x1
  def t: x.T
  def process(): Unit = x.process(t)

class Evil extends Z:
  def x2: X
  override val x: X = x2

// alarm bells should be ringing by now

// taking it to its conclusion...
object X1 extends X:
  override type T = Int
  override def process(t: T): Unit = println("Int: " + t)

object X2 extends X:
  override type T = String
  override def process(t: T): Unit = println("String: " + t)

@main def Test =
  new Evil{
    val x1 = X1
    val x2 = X2
    val t = 42  // error
  }.process() // BOOM: basically did x2.process(42)
