// scalajs: --skip

import scala.annotation.newMain

// Sample main method
object myProgram:

  /** Adds two numbers */
  @newMain def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /** Subtracts two numbers */
  @newMain def sub(num: Int, inc: Int): Unit =
    println(s"$num - $inc = ${num - inc}")

end myProgram

object Test:
  def callMain(mainMeth: String, args: Array[String]): Unit =
    val clazz = Class.forName(mainMeth)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain("add", Array("2", "3"))
    callMain("sub", Array("2", "3"))
end Test
