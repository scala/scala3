// Sample main method
object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

// Compiler generated code:
// TODO remove once @main generation is operational
object add extends main:
  def main(args: Array[String]) =
    val cmd = command(args)
    val arg1 = cmd.argGetter[Int]("num")
    val arg2 = cmd.argGetter[Int]("inc")
    cmd.run(myProgram.add(arg1(), arg2()), "add", "Adds two numbers")
end add

object Test:
  def main(args: Array[String]): Unit =
    add.main(Array("2", "3"))
end Test

// After implemented, move direct call to `add` in tests/neg

/** --- Some scenarios ----------------------------------------

> java add 2 3
2 + 3 = 5
> java add 4
4 + 1 = 5
> java add --num 10 --inc -2
10 + -2 = 8
> java add --num 10
10 + 1 = 11
> java add --help
Usage: add num inc?
Adds two numbers
> java add
Usage: add num inc?
Adds two numbers
> java add 1 2 3 4
Error: unused argument: 3
Error: unused argument: 4
Usage: add num inc?
> java add -n 1 -i 10
Error: invalid argument for num: -n
Error: unused argument: -i
Error: unused argument: 10
Usage: add num inc?
> java add --n 1 --i 10
Error: missing argument for num
Error: unknown argument name: --n
Error: unknown argument name: --i
Usage: add num inc?
> java add true 10
Error: invalid argument for num: true
Usage: add num inc?
> java add true false
Error: invalid argument for num: true
Error: invalid argument for inc: false
Usage: add num inc?
> java add true false 10
Error: invalid argument for num: true
Error: invalid argument for inc: false
Error: unused argument: 10
Usage: add num inc?
> java add --inc 10 --num 20
20 + 10 = 30
> java add binary 10 01
Error: invalid argument for num: binary
Error: unused argument: 01
Usage: add num inc?

*/
