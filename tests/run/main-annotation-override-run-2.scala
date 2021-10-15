class myMain extends main:
  override def run(f: => MainResultType): Unit =
    println("Yeah, I won't run that...")

object myProgram:

  /** Halt and catch fire */
  @myMain def hcf(code: Int): Nothing =
    throw new Exception("I should not be executed!!")

end myProgram

// TODO remove once @main generation is operational
object hcf extends myMain:
  def main(args: Array[String]) =
    val cmd = command(args, "hcf", "Halt and catch fire")
    val arg1 = cmd.argGetter[Int]("code")
    cmd.run(myProgram.hcf(arg1()))
end hcf

object Test:
  def main(args: Array[String]): Unit =
    hcf.main(Array("42"))
end Test
