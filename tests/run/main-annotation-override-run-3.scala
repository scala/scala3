class myMain extends main:
  override def run(f: => MainResultType): Unit =
    for (i <- 1 to 3)
      println(f)

object myProgram:
  var i = 0

  /* Get a new value on each call */
  @myMain def inc: Int =
    val inc = i
    i += 1
    inc

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("inc")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array())
end Test
