object myProgram:
  @main def altName1(
    @main.Alias("myNum") num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def altName2(
    @main.Alias("myNum") num: Int,
    @main.Alias("myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def shortName1(
    @main.Alias("n") num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def shortName2(
    @main.Alias("n") num: Int,
    @main.Alias("i") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def mix1(
    @main.Alias("myNum") @main.Alias("n") num: Int,
    @main.Alias("i") @main.Alias("myInc") inc: Int
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

  @main def mix2(
    @main.Alias(myNum) @main.Alias(myShortNum) num: Int,
    @main.Alias(myShortInc()) @main.Alias(myInc) inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def multiple(
    @main.Alias("myNum", "n") num: Int,
    @main.Alias("i", "myInc") inc: Int
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