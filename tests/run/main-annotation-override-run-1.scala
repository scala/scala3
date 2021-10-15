class myMain extends main:
  override def run(f: => MainResultType): Unit =
    println("I'm about to run!")
    f match {
      case main.ExitCode(n) => println(s"Exit with $n")
      case _ => println("I should not have printed this...")
    }
    println("I'm done!")

object myProgram:

  /** Adds two numbers */
  @myMain def add(num: Int, inc: Int): main.ExitCode =
    println(s"$num + $inc = ${num + inc}")
    main.ExitCode(num + inc)

end myProgram

// TODO remove once @main generation is operational
object add extends myMain:
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
