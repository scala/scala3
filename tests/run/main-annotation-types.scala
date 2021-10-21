// Sample main method
object myProgram:

  /** Displays some parameters */
  @main def show(
    int: Int,
    double: Double,
    string: String,
    boolean: Boolean
  ): Unit =
    println("Here's what I got:")
    println(s"int - $int")
    println(s"double - $double")
    println(s"string - $string")
    println(s"boolean - $boolean")
    println()

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3", "4", "true"))
    callMain(Array("-1", "3456789098765445678", "false", "FALSE"))
    callMain(Array("2147483647", "3.1415926535", "Hello world!", "True"))
end Test
