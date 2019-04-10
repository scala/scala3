import api._

object Test {
  def main(args: Array[String]): Unit = {
    assert(args(0).reflect == "args.apply(0)")
    assert(args(0).reflectColor.contains("\u001b")) // Contains Ansi color
  }
}
