// Sample main method
object myProgram:

  /** Adds two numbers and returns them */
  @main def add(num: Int, inc: Int): Int =
    println(f"$num + $inc = ${num + inc}")
    num + inc

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds two numbers and returns them")
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argGetter[Int]("inc")
    cmd.run(myProgram.add(arg1(), arg2()))
end add

object Test:
  def main(args: Array[String]): Unit =
    println("Direct call")
    println(myProgram.add(2, 3))
    println("Main call")
    println(add.main(Array("2", "3")))
end Test
