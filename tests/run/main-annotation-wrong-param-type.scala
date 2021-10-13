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
    val cmd = command(args)
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argGetter[Int]("inc")
    cmd.run(myProgram.add(arg1(), arg2()), "add", "Adds two numbers")
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("2", "true"))
    add.main(Array("2.1", "3"))
    add.main(Array("2", "3.1415921535"))
    add.main(Array("192.168.1.1", "3"))
    add.main(Array("false", "true"))
    add.main(Array("Hello", "world!"))
end Test
