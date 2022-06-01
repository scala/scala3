import scala.annotation.newMain
import scala.util.CommandLineParser.FromString

given intParser: FromString[Int => Int] with
  override def fromString(s: String) = n => summon[FromString[Int]].fromString(s) + n

given stringParser: FromString[String => String] with
  override def fromString(s: String) = s1 => summon[FromString[String]].fromString(s) + s1

object myProgram:

  @newMain def show(getI: Int => Int, getS: String => String) =
    println(getI(3))
    println(getS(" world!"))

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("show")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("39", "Hello"))
end Test
