package dotty.tools.repl

/** Main entry point to the REPL */
object Main {
  def main(args: Array[String]): Unit =
    new ReplDriver(args).runUntilQuit(needsTerminal = true)
}

object WorksheetMain {
  def main(args: Array[String]): Unit =
    new ReplDriver(args).runUntilQuit(needsTerminal = false)
}
