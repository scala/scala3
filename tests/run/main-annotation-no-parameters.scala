import scala.annotation.newMain

// Sample main method
object myProgram:

  /** Does nothing, except confirming that it runs */
  @newMain def run(): Unit =
    println("I run properly!")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("run")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array())
end Test
