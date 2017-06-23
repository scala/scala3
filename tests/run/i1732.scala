object Test {
  import scala.util.control.Breaks

  def brk(f: () => Unit): Unit = try {
    f()
  } catch {
    case ex: NotImplementedError =>
  }
  def main(args: Array[String]): Unit = {
    brk { () => ??? }
    Breaks.breakable {
      Breaks.break()
    }
  }
}
