class myMain extends main:
  override def usage(commandName: String, args: Seq[Argument]): Unit =
    val argInfos = args map (
      _ match {
        case SimpleArgument(name) => name
        case OptionalArgument(name, _) => s"[$name]"
        case VarArgument(name) => s"[$name [$name [...]]]"
      }
    )
    println(s"My shiny command works like this: $commandName ${argInfos.mkString(" ")}")

object myProgram:

  /** Adds two numbers */
  @myMain def add(num: Int, inc: Int*): Unit =
    println(s"$num + ${inc.mkString(" + ")} = ${num + inc.sum}")

end myProgram

// TODO remove once @main generation is operational
object add extends myMain:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds numbers")
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argsGetter[Int]("inc")
    cmd.run(myProgram.add(arg1(), arg2(): _*))
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("--help"))
end Test
