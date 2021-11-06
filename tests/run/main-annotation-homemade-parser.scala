import scala.util.CommandLineParser.FromString

class MyNumber(val value: Int) {
  def +(other: MyNumber): MyNumber = MyNumber(value + other.value)
}

given FromString[MyNumber] with
  def fromString(s: String): MyNumber = MyNumber(summon[FromString[Int]].fromString(s))

object myProgram:

  @main def add(num: MyNumber, inc: MyNumber): Unit =
    println(s"${num.value} + ${inc.value} = ${num.value + inc.value}")

end myProgram


object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("2", "3"))
end Test
