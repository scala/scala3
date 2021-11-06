// Sample main method
object myProgram:

  /** Adds two numbers and returns them */
  @main def add(num: Int, inc: Int): Int =
    println(f"$num + $inc = ${num + inc}")
    num + inc

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    println("Direct call")
    assert(myProgram.add(2, 3) == 5)
    println("Main call")
    callMain(Array("2", "3"))
end Test
