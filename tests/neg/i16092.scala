//> using options -source future -deprecation -Werror

trait X {
  type T
  def process(t: T): Unit
}

class Z(val x: X, val t: x.T) {
  def process(): Unit = x.process(t)
}
class Evil(x1: X, x2: X, t: x1.T) extends Z(x1, t) {
  override val x: X = x2 // error breaks connection between x and t
}
// alarm bells should be ringing by now

// taking it to its conclusion...
object x1 extends X {
  override type T = Int
  override def process(t: T): Unit = println("Int: " + t)
}
object x2 extends X {
  override type T = String
  override def process(t: T): Unit = println("String: " + t)
}

@main def Test = new Evil(x1, x2, 42).process() // BOOM: basically did x2.process(42)
