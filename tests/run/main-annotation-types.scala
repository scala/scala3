// Sample main method
object myProgram:

  /** Displays some parameters */
  @main def show(
    int: Int,
    double: Double,
    string: String,
    boolean: Boolean
  ): Unit =
    println("Here's what I got:")
    println(s"int - $int")
    println(s"double - $double")
    println(s"string - $string")
    println(s"boolean - $boolean")
    println()

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object show extends main:
  def main(args: Array[String]) =
    val cmd = command(args)
    val arg1 = cmd.argGetter[Int]("int")
    val arg2 = cmd.argGetter[Double]("double")
    val arg3 = cmd.argGetter[String]("string")
    val arg4 = cmd.argGetter[Boolean]("boolean")
    cmd.run(myProgram.show(arg1(), arg2(), arg3(), arg4()), "show", "Displays some parameters")
end show

object Test:
  def main(args: Array[String]): Unit =
    show.main(Array("2", "3", "4", "true"))
    show.main(Array("-1", "3456789098765445678", "false", "FALSE"))
    show.main(Array("2147483647", "3.1415926535", "Hello world!", "True"))
end Test
