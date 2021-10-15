class myMain extends main:
  override def explain(commandName: String, args: Seq[Argument], docComment: String): Unit =
    if docComment.nonEmpty then println(docComment.mkString("\n"))

object myProgram:

  /** Adds two numbers */
  @myMain def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

// TODO remove once @main generation is operational
object add extends myMain:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds two numbers")
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argGetter[Int]("inc", 1)
    cmd.run(myProgram.add(arg1(), arg2()))
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("--help"))
end Test
