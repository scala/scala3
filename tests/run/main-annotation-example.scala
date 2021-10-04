import scala.annotation.*
import collection.mutable
import scala.util.CommandLineParser.FromString

/** Sum all the numbers
 *
 *  @param first Fist number to sum
 *  @param rest The rest of the numbers to sum
 */
@myMain def sum(first: Int, rest: Int*): Int = first + rest.sum


object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("sum")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("23", "2", "3"))
end Test

@experimental
class myMain extends MainAnnotation:
  import MainAnnotation.{ Command, CommandInfo, ParameterInfo }

  /** A new command with arguments from `args` */
  def command(info: CommandInfo, args: Array[String]): Command[FromString, Int] =
    if args.contains("--help") then
      println(info.documentation)
      System.exit(0)
    assert(info.parameters.forall(!_.hasDefault), "Default arguments are not supported")
    val (plainArgs, varargs) =
      if info.parameters.last.isVarargs then
        val numPlainArgs = info.parameters.length - 1
        assert(numPlainArgs <= args.length, "Not enough arguments")
        (args.take(numPlainArgs), args.drop(numPlainArgs))
      else
        assert(info.parameters.length <= args.length, "Not enough arguments")
        assert(info.parameters.length >= args.length, "Too many arguments")
        (args, Array.empty[String])
    new MyCommand(plainArgs, varargs)

  @experimental
  class MyCommand(plainArgs: Seq[String], varargs: Seq[String]) extends Command[FromString, Int]:

    def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using parser: FromString[T]): () => T =
      () => parser.fromString(plainArgs(idx))

    def varargGetter[T](using parser: FromString[T]): () => Seq[T] =
      () => varargs.map(arg => parser.fromString(arg))

    def run(program: () => Int): Unit =
      println("executing program")
      val result = program()
      println("result: " + result)
      println("executed program")
  end MyCommand
end myMain
