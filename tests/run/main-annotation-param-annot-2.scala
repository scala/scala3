object myProgram:
  @main def multipleSameShortNames1(
    @main.ShortName('n') num: Int,
    @main.ShortName('n') inc: Int
  ): Unit = ()

  @main def multipleSameShortNames2(
    @main.ShortName('n') @main.ShortName('n') num: Int,
    inc: Int
  ): Unit = ()

  @main def multipleSameNames1(
    @main.Name("arg") num: Int,
    @main.Name("arg") inc: Int
  ): Unit = ()

  @main def multipleSameNames2(
    @main.Name("arg") @main.Name("arg") num: Int,
    inc: Int
  ): Unit = ()

  @main def multipleSameNames3(
    num: Int,
    @main.Name("num") inc: Int
  ): Unit = ()
end myProgram


object Test:
  def hasCauseAssertionError(e: Throwable): Boolean =
    e.getCause match {
      case null => false
      case _: AssertionError => true
      case e: Throwable => hasCauseAssertionError(e)
    }

  def callMain(className: String, args: Array[String]) =
    val clazz = Class.forName(className)
    val method = clazz.getMethod("main", classOf[Array[String]])

    try { method.invoke(null, args) }
    catch {
      case e: Exception if hasCauseAssertionError(e) => println("OK")
    }

  def main(args: Array[String]): Unit =
    callMain("multipleSameShortNames1", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameShortNames1", Array("-n", "2", "--inc", "3"))
    callMain("multipleSameShortNames2", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameShortNames2", Array("-n", "2", "--inc", "3"))

    callMain("multipleSameNames1", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameNames1", Array("--arg", "2", "--inc", "3"))
    callMain("multipleSameNames2", Array("--num", "2", "--inc", "3"))
    callMain("multipleSameNames2", Array("--arg", "2", "--inc", "3"))
    callMain("multipleSameNames3", Array("--num", "2", "--inc", "3"))
end Test