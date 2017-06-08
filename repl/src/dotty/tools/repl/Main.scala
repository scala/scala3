package dotty.tools.repl

object Main {
  def main(args: Array[String]): Unit =
    new Repl(args.toList).run()
}
