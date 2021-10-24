class myMain extends main:
  override def explain(commandName: String, docComment: String, args: Seq[Argument]): Unit =
    if docComment.nonEmpty then println(docComment.mkString("\n"))

object myProgram:

  /** Adds two numbers */
  @myMain def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("--help"))
end Test
