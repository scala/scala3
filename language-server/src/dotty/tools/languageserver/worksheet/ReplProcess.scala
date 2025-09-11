package dotty.tools.languageserver.worksheet

import dotty.tools.repl.ReplDriver

object ReplProcess {
  def main(args: Array[String]): Unit = {
    val driver = new ReplDriver(args, extraPredef = ReplDriver.pprintImport)
    val in = new InputStreamConsumer(System.in)
    var state = driver.initialState

    while (true) {
      val code = in.next() // blocking
      state = driver.run(code)(using state)
      Console.print(InputStreamConsumer.delimiter) // needed to mark the end of REPL output
      Console.flush()
    }
  }
}
