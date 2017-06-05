package dotty.tools.repl

object Main {
  private[this] val reader = new AmmoniteReader

  def main(args: Array[String]): Unit =
    reader.prompt()
}
