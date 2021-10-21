// Sample main method
object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("-n", "1", "-i", "10"))
    callMain(Array("--n", "1", "--i", "10"))
    callMain(Array("-num", "1", "--inc", "10"))
    callMain(Array("--num", "1", "-inc", "10"))
end Test
