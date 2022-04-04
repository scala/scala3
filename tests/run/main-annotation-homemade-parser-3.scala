import scala.annotation.newMain
import scala.util.CommandLineParser.FromString

given FromString[Int] with
  override def fromString(s: String) = s.toInt + 42

object myProgram:

  given FromString[Int] with
    override def fromString(s: String) = -1 * s.toInt // Should be ignored, because not top-level

  @newMain def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram


object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
end Test
