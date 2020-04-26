import annotation.{Annotation, StaticAnnotation}
import collection.mutable

/** This class provides a framework for compiler-generated wrappers
 *  of "entry-point" methods. It routes and transforms parameters and results
 *  between a compiler-generated wrapper method that has calling conventions
 *  fixed by a framework and a user-written entry-point method that can have
 *  flexible argument lists. It allows the wrapper to provide help and usage
 *  information as well as customised error messages if the actual wrapper arguments
 *  do not match the expected entry-point parameters.
 *
 *  The protocol of calls from the wrapper method is as follows:
 *
 *    1. Create a `call` instance with the wrapper argument.
 *    2. For each parameter of the entry-point, invoke `call.nextArgGetter`,
 *       or `call.finalArgsGetter` if is a final varargs parameter.
 *    3. Invoke `call.run` with the closure of entry-point applied to all arguments.
 *
 *  The wrapper class has this outline:
 *
 *     object <wrapperClass>:
 *       @WrapperAnnotation def <wrapperMethod>(args: <WrapperArgs>) =
 *         ...
 *
 *  Here `<wrapperClass>` and `<wrapperMethod>` are obtained from an
 *  inline call to the `wrapperName` method.
 */
trait EntryPointAnnotation extends StaticAnnotation:

  /** The class used for argument parsing. E.g. `scala.util.FromString`, if
   *  arguments are strings, but it could be something else.
   */
  type ArgumentParser[T]

  /** The required result type of the user-defined main function */
  type EntryPointResult

  /** The type of the wrapper argument. E.g., for Java main methods: `Array[String]` */
  type WrapperArgs

  /** The return type of the generated wrapper. E.g., for Java main methods: `Unit` */
  type WrapperResult

  /** An annotation type with which the wrapper method is decorated.
   *  No annotation is generated if the type is left abstract.
   */
  type WrapperAnnotation <: Annotation

  /** The fully qualified name (relative to enclosing package) to
   *  use for the static wrapper method.
   *  @param mainName the fully qualified name of the user-defined main method
   */
  inline def wrapperName(mainName: String): String

  /** A new wrapper call with arguments from `args` */
  def call(args: WrapperArgs): Call

  /** A class representing a wrapper call */
  abstract class Call:

    /** The getter for the next argument of type `T` */
    def nextArgGetter[T](argName: String, fromString: ArgumentParser[T], defaultValue: Option[T] = None): () => T

    /** The getter for a final varargs argument of type `T*` */
    def finalArgsGetter[T](argName: String, fromString: ArgumentParser[T]): () => Seq[T]

    /** Run `entryPoint` if all arguments are valid,
     *  or print usage information and/or error messages.
     *  @param entryPointApply the applied entry-point to run
     *  @param entryPointName  the fully qualified name of the entry-point method
     *  @param docComment      the doc comment of the entry-point method
     */
    def run(entryPointApply: => EntryPointResult, entryPointName: String, docComment: String): WrapperResult
  end Call
end EntryPointAnnotation

//Sample main class, can be freely implemented:

class main extends EntryPointAnnotation:

  type ArgumentParser[T] = util.FromString[T]
  type EntryPointResult  = Unit
  type WrapperArgs       = Array[String]
  type WrapperResult     = Unit

  def call(args: Array[String]) = new Call:

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

    def run(entryPointApply: => EntryPointResult, entryPointName: String, docComment: String): Unit =
      def usage(): Unit =
        val cmd = entryPointName.dropRight(".main".length)
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
        else entryPointApply
    end run
  end call

  inline def wrapperName(mainName: String): String =
    s"${mainName.drop(mainName.lastIndexOf('.') + 1)}.main"

end main

// Sample main method

object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

//  Compiler generated code:

object add extends main:
  def main(args: Array[String]) =
    val cll = call(args)
    val arg1 = cll.nextArgGetter[Int]("num", summon[ArgumentParser[Int]])
    val arg2 = cll.nextArgGetter[Int]("inc", summon[ArgumentParser[Int]], Some(1))
    cll.run(myProgram.add(arg1(), arg2()), "add", "Adds two numbers")
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