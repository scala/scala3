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

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
end Test
