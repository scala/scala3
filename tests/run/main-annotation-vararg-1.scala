// scalajs: --skip

import scala.annotation.newMain

// Sample main method
object myProgram:

  /** Adds any amount of numbers */
  @newMain def add(nums: Int*): Unit =
    if (nums.isEmpty)
      println("No number input")
    else
      println(s"${nums.mkString(" + ")} = ${nums.sum}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
    callMain(Array("2", "3", "-4"))
    callMain((1 to 10).toArray.map(_.toString))
    callMain(Array("0"))
    callMain(Array())
end Test
