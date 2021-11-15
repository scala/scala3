import scala.util.CommandLineParser.FromString

object myProgram:

  @main def add(num: Test.MyNumber, inc: Test.MyNumber): Unit = // error
    val numV = Test.value(num)
    val incV = Test.value(inc)
    println(s"$numV + $incV = ${numV + incV}")

end myProgram


object Test:
  opaque type MyNumber = Int

  def create(n: Int): MyNumber = n
  def value(n: MyNumber): Int = n

  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
end Test
