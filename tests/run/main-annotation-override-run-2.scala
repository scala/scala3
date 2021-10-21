class myMain extends main:
  override def run(f: => MainResultType): Unit =
    println("Yeah, I won't run that...")

object myProgram:

  /** Halt and catch fire */
  @myMain def hcf(code: Int): Nothing =
    throw new Exception("I should not be executed!!")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("hcf")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("42"))
end Test
