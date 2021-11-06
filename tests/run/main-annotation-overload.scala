// Sample main method
object myProgram:

  /** Adds three numbers (malformed, doesn't work) */
  def add(num1: Int, num2: Int, num3: Int): Unit =
    ???

  /** Adds two numbers */
  @main def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

  /** Adds one number (malformed, doesn't work) */
  def add(num: Int): Unit =
    ???

  /** Adds zero numbers (malformed, doesn't work) */
  def add(): Int =
    ???

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
end Test
