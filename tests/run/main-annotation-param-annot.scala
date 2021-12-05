object myProgram:
  @main def noParamAnnotArgs(
    @main.Arg() num: Int,
    @main.Arg() inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def altName1(
    @main.Arg(name = "myNum") num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def altName2(
    @main.Arg(name = "myNum") num: Int,
    @main.Arg(name = "myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def shortName1(
    @main.Arg(shortName = 'n') num: Int,
    inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def shortName2(
    @main.Arg(shortName = 'n') num: Int,
    @main.Arg(shortName = 'i') inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def mix(
    @main.Arg(name = "myNum", shortName = 'n') num: Int,
    @main.Arg(shortName = 'i', name = "myInc") inc: Int
  ): Unit =
    println(s"$num + $inc = ${num + inc}")

  @main def multipleSameShortName(
    @main.Arg(shortName = 'n') num: Int,
    @main.Arg(shortName = 'n') inc: Int
  ): Unit = ()
end myProgram


object Test:
  def callMain(className: String, args: Array[String]) =
    val clazz = Class.forName(className)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain("noParamAnnotArgs", Array("--num", "2", "--inc", "3"))

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

    callMain("multipleSameShortName", Array("2", "3"))
end Test