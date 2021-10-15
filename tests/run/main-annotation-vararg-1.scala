// Sample main method
object myProgram:

  /** Adds any amount of numbers */
  @main def add(nums: Int*): Unit =
    if (nums.isEmpty)
      println("No number input")
    else
      println(s"${nums.mkString(" + ")} = ${nums.sum}")

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds any amount of numbers")
    val arg1 = cmd.argsGetter[Int]("nums")
    cmd.run(myProgram.add(arg1(): _*))
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("2", "3"))
    add.main(Array("2", "3", "-4"))
    add.main((1 to 10).toArray.map(_.toString))
    add.main(Array("0"))
    add.main(Array())
end Test
