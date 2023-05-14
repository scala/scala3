object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: => Int): Unit =  // error
    println(s"$num + $inc = ${num + inc}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
end Test
