// Sample main method
object myProgram:

  /** Does nothing, except confirming that it runs */
  @main def run(): Unit =
    println("I run properly!")

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object run extends main:
  def main(args: Array[String]) =
    val cmd = command(args, "run", "Does nothing, except confirming that it runs")
    cmd.run(myProgram.run())
end run

object Test:
  def main(args: Array[String]): Unit =
    run.main(Array())
end Test
