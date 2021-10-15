// Sample main method
object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds two numbers")
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argGetter[Int]("inc")
    cmd.run(myProgram.add(arg1(), arg2()))
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("--num", "2", "--inc", "3"))
    add.main(Array("--inc", "3", "--num", "2"))
end Test