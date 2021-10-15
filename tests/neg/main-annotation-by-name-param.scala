object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: => Int): Unit =  // error
    println(s"$num + $inc = ${num + inc}")

end myProgram

object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds two numbers")
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argGetter[Int]("inc")
    cmd.run(myProgram.add(arg1(), arg2()))
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("2", "3"))
end Test
