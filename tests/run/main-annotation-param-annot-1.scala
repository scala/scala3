object myProgram:
  @main def altName1(
    @main.Name("myNum") num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def altName2(
    @main.Name("myNum") num: Int,
    @main.Name("myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def shortName1(
    @main.ShortName('n') num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def shortName2(
    @main.ShortName('n') num: Int,
    @main.ShortName('i') inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def mix(
    @main.Name("myNum") @main.ShortName('n') num: Int,
    @main.ShortName('i') @main.Name("myInc") inc: Int
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

    callMain("mix", Array("--num", "2", "--inc", "3"))
    callMain("mix", Array("-n", "2", "--inc", "3"))
    callMain("mix", Array("--num", "2", "-i", "3"))
    callMain("mix", Array("-n", "2", "-i", "3"))
    callMain("mix", Array("--myNum", "2", "--myInc", "3"))
    callMain("mix", Array("-n", "2", "--myInc", "3"))
    callMain("mix", Array("--myNum", "2", "-i", "3"))
    callMain("mix", Array("-n", "2", "-i", "3"))
end Test