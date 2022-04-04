import scala.annotation.newMain

// Sample main method
object myProgram:

  @newMain def alwaysPassParam(forbiddenParam: Int = throw new IllegalStateException("This should not be evaluated!")): Unit =
    println(forbiddenParam)

end myProgram

object Test:
  def hasCauseIllegalStateException(e: Throwable): Boolean =
    e.getCause match {
      case null => false
      case _: IllegalStateException => true
      case e: Throwable => hasCauseIllegalStateException(e)
    }

  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("alwaysPassParam")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("42"))
    try {
      callMain(Array())
      println("This should not be printed")
    }
    catch {
      case e: Exception if hasCauseIllegalStateException(e) => println("OK")
    }
end Test
