// Sample main method
object myProgram:

  /** Exits program with error code */
  @main def exit(code: Int): main.ExitCode =
    println(f"Exiting with code $code")
    main.ExitCode(code)

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object exit extends main:
  def main(args: Array[String]) =
    val cmd = command(args)
    val arg1 = cmd.argGetter[Int]("code")
    cmd.run(myProgram.exit(arg1()), "exit", "Exits program with error code")
end exit

object Test:
  def main(args: Array[String]): Unit =
    exit.main(Array("42"))
end Test
