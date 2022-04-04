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
    callMain(Array("2", "true"))
    callMain(Array("2.1", "3"))
    callMain(Array("2", "3.1415921535"))
    callMain(Array("192.168.1.1", "3"))
    callMain(Array("false", "true"))
    callMain(Array("Hello", "world!"))
end Test
