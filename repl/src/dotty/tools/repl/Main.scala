package dotty.tools
package repl

/** Main entry point to the REPL */
// To test, run `sbt buildQuick` then `bin/replQ`
object Main {
  def main(args: Array[String]): Unit =
    new ReplDriver(args, extraPredef = ReplDriver.pprintImport).tryRunning
}
