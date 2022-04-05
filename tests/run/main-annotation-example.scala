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
class myMain extends MainAnnotation[FromString, Int]:
  import MainAnnotation.{ Info, Parameter }

  def command(info: Info, args: Seq[String]): Option[Seq[String]] =
    if args.contains("--help") then
      println(info.documentation)
      None // do not parse or run the program
    else if info.parameters.exists(_.hasDefault) then
      println("Default arguments are not supported")
      None
    else if info.hasVarargs then
      val numPlainArgs = info.parameters.length - 1
      if numPlainArgs > args.length then
        println("Not enough arguments")
        None
      else
        Some(args)
    else
      if info.parameters.length > args.length then
        println("Not enough arguments")
        None
      else if info.parameters.length < args.length then
        println("Too many arguments")
        None
      else
        Some(args)

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using parser: FromString[T]): () => T =
    () => parser.fromString(arg)

  def varargGetter[T](param: Parameter, args: Seq[String])(using parser: FromString[T]): () => Seq[T] =
    () => args.map(arg => parser.fromString(arg))

  def run(program: () => Int): Unit =
    println("executing program")
    val result = program()
    println("result: " + result)
    println("executed program")
end myMain
