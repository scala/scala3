// Sample main method
object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args)
    val arg1 = cmd.argGetter[Int]("num", summon[ArgumentParser[Int]])
    val arg2 = cmd.argGetter[Int]("inc", summon[ArgumentParser[Int]], Some(1))
    cmd.run(myProgram.add(arg1(), arg2()), "add", "Adds two numbers")
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("--help"))
    add.main(Array("Some", "garbage", "before", "--help"))
    add.main(Array("--help", "and", "some", "stuff", "after"))
end Test
