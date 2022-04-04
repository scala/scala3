import scala.annotation.newMain

// Sample main method
object myProgram:

  /** Adds two numbers */
  @newMain def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("--num", "2", "--inc", "3"))
    callMain(Array("--inc", "3", "--num", "2"))

    callMain(Array("2", "--inc", "3"))
    callMain(Array("--num", "2", "3"))

    callMain(Array("--num", "2", "--num", "1", "--inc", "3"))
    callMain(Array("--inc", "1", "--num", "2", "--num", "1", "--inc", "3"))
end Test
