class MyNumber(val value: Int) {
  def +(other: MyNumber): MyNumber = MyNumber(value + other.value)
}

object myProgram:

  // The following line will produce an error when the main class is generated dynamically
  @main def add(num: MyNumber, inc: MyNumber): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args, "add", "Adds two numbers")
    // TODO remove errors below when code is generated
    val arg1 = cmd.argGetter[MyNumber]("num") // error
    val arg2 = cmd.argGetter[MyNumber]("inc") // error
    cmd.run(myProgram.add(arg1(), arg2()))
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("2", "3"))
end Test
