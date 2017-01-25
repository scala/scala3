trait Break { protected val break: Int; }
case class BreakImpl(protected val break: Int) extends Break {}
object Test {
  def f2(x: Break) = x match {
    case BreakImpl(x) => BreakImpl
    case _ => -1
  }
  def f4(x: Any) = x match {
    case BreakImpl(x) => x
    case _ => -1
  }
  def main(args: Array[String]): Unit = {
    val break = BreakImpl(22)
    assert(f2(break) == 22)
    assert(f4(break) == 22)
  }
}
