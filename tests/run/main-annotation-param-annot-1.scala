import scala.annotation.newMain
import scala.annotation.newMain.alias

object myProgram:
  @newMain def altName1(
    @alias("myNum") num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @newMain def altName2(
    @alias("myNum") num: Int,
    @alias("myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @newMain def shortName1(
    @alias("n") num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @newMain def shortName2(
    @alias("n") num: Int,
    @alias("i") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @newMain def mix1(
    @alias("myNum") @alias("n") num: Int,
    @alias("i") @alias("myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  def myNum: String = "myNum"
  def myShortNum = {
    var short = 'a'
    for i <- 0 until 'n' - 'a'
    do
      short = (short.toInt + 1).toChar
    short.toString
  }
  def myInc = {new Exception("myInc")}.getMessage
  def myShortInc = () => "i"

  @newMain def mix2(
    @alias(myNum) @alias(myShortNum) num: Int,
    @alias(myShortInc()) @alias(myInc) inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @newMain def multiple(
    @alias("myNum") @alias("n") num: Int,
    @alias("i") @alias("myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")
end myProgram


object Test:
  def callMain(className: String, args: Array[String]) =
    val clazz = Class.forName(className)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain("altName1", Array("--num", "2", "--inc", "3"))
    callMain("altName1", Array("--myNum", "2", "--inc", "3"))

    callMain("altName2", Array("--num", "2", "--inc", "3"))
    callMain("altName2", Array("--myNum", "2", "--inc", "3"))
    callMain("altName2", Array("--num", "2", "--myInc", "3"))
    callMain("altName2", Array("--myNum", "2", "--myInc", "3"))

    callMain("shortName1", Array("--num", "2", "--inc", "3"))
    callMain("shortName1", Array("-n", "2", "--inc", "3"))

    callMain("shortName2", Array("--num", "2", "--inc", "3"))
    callMain("shortName2", Array("-n", "2", "--inc", "3"))
    callMain("shortName2", Array("--num", "2", "-i", "3"))
    callMain("shortName2", Array("-n", "2", "-i", "3"))

    callMain("mix1", Array("--num", "2", "--inc", "3"))
    callMain("mix1", Array("-n", "2", "--inc", "3"))
    callMain("mix1", Array("--num", "2", "-i", "3"))
    callMain("mix1", Array("-n", "2", "-i", "3"))
    callMain("mix1", Array("--myNum", "2", "--myInc", "3"))
    callMain("mix1", Array("-n", "2", "--myInc", "3"))
    callMain("mix1", Array("--myNum", "2", "-i", "3"))
    callMain("mix1", Array("-n", "2", "-i", "3"))
    callMain("mix2", Array("--num", "2", "--inc", "3"))
    callMain("mix2", Array("-n", "2", "--inc", "3"))
    callMain("mix2", Array("--num", "2", "-i", "3"))
    callMain("mix2", Array("-n", "2", "-i", "3"))
    callMain("mix2", Array("--myNum", "2", "--myInc", "3"))
    callMain("mix2", Array("-n", "2", "--myInc", "3"))
    callMain("mix2", Array("--myNum", "2", "-i", "3"))
    callMain("mix2", Array("-n", "2", "-i", "3"))

    callMain("multiple", Array("--num", "2", "--inc", "3"))
    callMain("multiple", Array("-n", "2", "--inc", "3"))
    callMain("multiple", Array("--num", "2", "-i", "3"))
    callMain("multiple", Array("-n", "2", "-i", "3"))
    callMain("multiple", Array("--myNum", "2", "--myInc", "3"))
    callMain("multiple", Array("-n", "2", "--myInc", "3"))
    callMain("multiple", Array("--myNum", "2", "-i", "3"))
    callMain("multiple", Array("-n", "2", "-i", "3"))
end Test
