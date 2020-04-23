import annotation.{Annotation, StaticAnnotation}
import collection.mutable

/** MainAnnotation provides the functionality for a compiler-generated wrapper class.
 *  It links a compiler-generated main method (call it compiler-main) to a user
 *  written main method (user-main).
 *  The protocol of calls from compiler-main is as follows:
 *
 *    - create a `command` with the command line arguments,
 *    - for each parameter of user-main, a call to `command.nextArgGetter`,
 *      or `command.finalArgsGetter` if is a final varargs parameter,
 *    - a call to `command.run` with the closure of user-main applied to all arguments.
 *
 *  The wrapper class has this outline:
 *
 *     object <wrapperClass>:
 *       def <wrapperMethod>(args: <CommandLineArgs>) =
 *         ...
 *
 *  Here the `<wrapperClass>` name is the result of an inline call to `wrapperClassName`
 *  and `<wrapperMethod>` is the result of an inline call to `wrapperMethodName`
 *
 *  The result type of `<main>` is the same as the result type of `run`
 *  in the concrete implementation of `MainAnnotation`.
 */
trait MainAnnotation extends StaticAnnotation:

  /** The class used for argument string parsing. E.g. `scala.util.FromString`,
   *  but could be something else
   */
  type ArgumentParser[T]

  /** The required result type of the user-defined main function */
  type MainResultType

  /** The type of the command line arguments. E.g., for Java main methods: `Array[String]` */
  type CommandLineArgs

  /** The return type of the generated command. E.g., for Java main methods: `Unit` */
  type CommandResult

  /** A new command with arguments from `args` */
  def command(args: CommandLineArgs): Command

  /** A class representing a command to run */
  abstract class Command:

    /** The getter for the next argument of type `T` */
    def nextArgGetter[T](argName: String, fromString: ArgumentParser[T], defaultValue: Option[T] = None): () => T

    /** The getter for a final varargs argument of type `T*` */
    def finalArgsGetter[T](argName: String, fromString: ArgumentParser[T]): () => Seq[T]

    /** Run `program` if all arguments are valid,
     *  or print usage information and/or error messages.
     *  @param program    the program to run
     *  @param mainName   the fully qualified name of the user-defined main method
     *  @param docComment the doc comment of the user-defined main method
     */
    def run(program: => MainResultType, mainName: String, docComment: String): CommandResult
  end Command

  /** The fully qualified name to use for the static wrapper method
   *  @param mainName the fully qualified name of the user-defined main method
   */
  inline def wrapperName(mainName: String): String

  /** An annotation type with which the wrapper method is decorated.
   *  No annotation is generated if the type is left abstract.
   */
  type WrapperAnnotation <: Annotation

end MainAnnotation

//Sample main class, can be freely implemented:

class main extends MainAnnotation:

  type ArgumentParser[T] = util.FromString[T]
  type MainResultType = Any
  type CommandLineArgs = Array[String]
  type CommandResult = Unit

  def command(args: Array[String]) = new Command:

    /** A buffer of demanded argument names, plus
     *   "?"  if it has a default
     *   "*"  if it is a vararg
     *   ""   otherwise
     */
    private var argInfos = new mutable.ListBuffer[(String, String)]

    /** A buffer for all errors */
    private var errors = new mutable.ListBuffer[String]

    /** Issue an error, and return an uncallable getter */
    private def error(msg: String): () => Nothing =
      errors += msg
      () => assertFail("trying to get invalid argument")

    /** The next argument index */
    private var argIdx: Int = 0

    private def argAt(idx: Int): Option[String] =
      if idx < args.length then Some(args(idx)) else None

    private def nextPositionalArg(): Option[String] =
      while argIdx < args.length && args(argIdx).startsWith("--") do argIdx += 2
      val result = argAt(argIdx)
      argIdx += 1
      result

    private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
      p.fromStringOption(arg) match
        case Some(t) => () => t
        case None => error(s"invalid argument for $argName: $arg")

    def nextArgGetter[T](argName: String, p: ArgumentParser[T], defaultValue: Option[T] = None): () => T =
      argInfos += ((argName, if defaultValue.isDefined then "?" else ""))
      val idx = args.indexOf(s"--$argName")
      val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
      argOpt match
        case Some(arg) => convert(argName, arg, p)
        case None => defaultValue match
          case Some(t) => () => t
          case None => error(s"missing argument for $argName")

    def finalArgsGetter[T](argName: String, p: ArgumentParser[T]): () => Seq[T] =
      argInfos += ((argName, "*"))
      def remainingArgGetters(): List[() => T] = nextPositionalArg() match
        case Some(arg) => convert(arg, argName, p) :: remainingArgGetters()
        case None => Nil
      val getters = remainingArgGetters()
      () => getters.map(_())

    def run(f: => MainResultType, mainName: String, docComment: String): Unit =
      def usage(): Unit =
        val cmd = mainName.dropRight(".main".length)
        val params = argInfos.map(_ + _).mkString(" ")
        println(s"Usage: java $cmd $params")

      def explain(): Unit =
        if docComment.nonEmpty then println(docComment)  // todo: process & format doc comment

      def flagUnused(): Unit = nextPositionalArg() match
        case Some(arg) =>
          error(s"unused argument: $arg")
          flagUnused()
        case None =>
          for
            arg <- args
            if arg.startsWith("--") && !argInfos.map(_._1).contains(arg.drop(2))
          do
            error(s"unknown argument name: $arg")
      end flagUnused

      if args.isEmpty || args.contains("--help") then
        usage()
        explain()
      else
        flagUnused()
        if errors.nonEmpty then
          for msg <- errors do println(s"Error: $msg")
          usage()
        else f match
          case n: Int if n < 0 => System.exit(-n)
          case _ =>
    end run
  end command

  inline def wrapperName(mainName: String): String =
    s"${mainName.drop(mainName.lastIndexOf('.') + 1)}.main"

  override type WrapperAnnotation = EntryPoint

end main

class EntryPoint extends Annotation

// Sample main method

object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

//  Compiler generated code:

object add extends main:
  @EntryPoint def main(args: Array[String]) =
    val cmd = command(args)
    val arg1 = cmd.nextArgGetter[Int]("num", summon[ArgumentParser[Int]])
    val arg2 = cmd.nextArgGetter[Int]("inc", summon[ArgumentParser[Int]], Some(1))
    cmd.run(myProgram.add(arg1(), arg2()), "add", "Adds two numbers")
end add

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
Usage: java add num inc?
Adds two numbers
> java add
Usage: java add num inc?
Adds two numbers
> java add 1 2 3 4
Error: unused argument: 3
Error: unused argument: 4
Usage: java add num inc?
> java add -n 1 -i 10
Error: invalid argument for num: -n
Error: unused argument: -i
Error: unused argument: 10
Usage: java add num inc?
> java add --n 1 --i 10
Error: missing argument for num
Error: unknown argument name: --n
Error: unknown argument name: --i
Usage: java add num inc?
> java add true 10
Error: invalid argument for num: true
Usage: java add num inc?
> java add true false
Error: invalid argument for num: true
Error: invalid argument for inc: false
Usage: java add num inc?
> java add true false 10
Error: invalid argument for num: true
Error: invalid argument for inc: false
Error: unused argument: 10
Usage: java add num inc?
> java add --inc 10 --num 20
20 + 10 = 30
> java add binary 10 01
Error: invalid argument for num: binary
Error: unused argument: 01
Usage: java add num inc?

*/