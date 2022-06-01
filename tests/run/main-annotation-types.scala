import scala.annotation.newMain

// Sample main method
object myProgram:

  /** Displays some parameters */
  @newMain def show(
    int: Int,
    double: Double,
    string: String,
    byte: Byte
  ): Unit =
    println("Here's what I got:")
    println(s"int - $int")
    println(s"double - $double")
    println(s"string - $string")
    println(s"byte - $byte")
    println()

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("show")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3", "4", "1"))
    callMain(Array("-1", "3456789098765445678", "false", "127"))
    callMain(Array("2147483647", "3.1415926535", "Hello world!", "0"))
end Test
