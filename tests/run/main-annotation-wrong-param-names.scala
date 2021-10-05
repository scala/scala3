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
    val arg1 = cmd.argGetter[Int]("num", summon[ArgumentParser[Int]])
    val arg2 = cmd.argGetter[Int]("inc", summon[ArgumentParser[Int]])
    cmd.run(myProgram.add(arg1(), arg2()), "add", "Adds two numbers")
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("-n", "1", "-i", "10"))
    add.main(Array("--n", "1", "--i", "10"))
end Test
