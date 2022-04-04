import scala.annotation.newMain

class MyResult(val result: Int):
  override def toString: String = result.toString

// Sample main method
object myProgram:

  /** Adds two numbers and returns them */
  @newMain def add(num: Int, inc: Int) =
    println(MyResult(num + inc))

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    println("Direct call")
    myProgram.add(2, 3)
    println("Main call")
    callMain(Array("2", "3"))
end Test
