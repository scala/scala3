import annotation.StaticAnnotation
import collection.mutable

trait MainAnnotation extends StaticAnnotation:

  type ArgumentParser[T]

  // get single argument
  def getArg[T](argName: String, fromString: ArgumentParser[T], defaultValue: Option[T] = None): () => T

  // get varargs argument
  def getArgs[T](argName: String, fromString: ArgumentParser[T]): () => List[T]

  // check that everything is parsed
  def done(): Boolean

end MainAnnotation

//Sample main class, can be freely implemented:

class main(progName: String, args: Array[String], docComment: String) extends MainAnnotation:

  def this() = this("", Array(), "")

  type ArgumentParser[T] = util.CommandLineParser.FromString[T]

  /** A buffer of demanded argument names, plus
   *   "?"  if it has a default
   *   "*"  if it is a vararg
   *   ""   otherwise
   */
  private var argInfos = new mutable.ListBuffer[(String, String)]

  /** A buffer for all errors */
  private var errors = new mutable.ListBuffer[String]

  /** The next argument index */
  private var n: Int = 0

  private def error(msg: String): () => Nothing =
    errors += msg
    () => ???

  private def argAt(idx: Int): Option[String] =
    if idx < args.length then Some(args(idx)) else None

  private def nextPositionalArg(): Option[String] =
    while n < args.length && args(n).startsWith("--") do n += 2
    val result = argAt(n)
    n += 1
    result

  private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
    p.fromStringOption(arg) match
      case Some(t) => () => t
      case None => error(s"invalid argument for $argName: $arg")

  def getArg[T](argName: String, p: ArgumentParser[T], defaultValue: Option[T] = None): () => T =
    argInfos += ((argName, if defaultValue.isDefined then "?" else ""))
    val idx = args.indexOf(s"--$argName")
    val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
    argOpt match
      case Some(arg) => convert(argName, arg, p)
      case None => defaultValue match
        case Some(t) => () => t
        case None => error(s"missing argument for $argName")

  def getArgs[T](argName: String, fromString: ArgumentParser[T]): () => List[T] =
    argInfos += ((argName, "*"))
    def recur(): List[() => T] = nextPositionalArg() match
      case Some(arg) => convert(arg, argName, fromString) :: recur()
      case None => Nil
    val fns = recur()
    () => fns.map(_())

  def usage(): Boolean =
    println(s"Usage: $progName ${argInfos.map(_ + _).mkString(" ")}")
    if docComment.nonEmpty then
      println(docComment)  // todo: process & format doc comment
    false

  def showUnused(): Unit = nextPositionalArg() match
    case Some(arg) =>
      error(s"unused argument: $arg")
      showUnused()
    case None =>
      for
        arg <- args
        if arg.startsWith("--") && !argInfos.map(_._1).contains(arg.drop(2))
      do
        error(s"unknown argument name: $arg")
  end showUnused

  def done(): Boolean =
    if args.contains("--help") then
      usage()
    else
      showUnused()
      if errors.nonEmpty then
        for msg <- errors do println(s"Error: $msg")
        usage()
      else
        true
  end done
end main

// Sample main method

object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int = 1) =
    println(s"$num + $inc = ${num + inc}")

end myProgram

//  Compiler generated code:

object add:
  def main(args: Array[String]) =
    val cmd = new main("add", args, "Adds two numbers")
    val arg1 = cmd.getArg[Int]("num", summon[cmd.ArgumentParser[Int]])
    val arg2 = cmd.getArg[Int]("inc", summon[cmd.ArgumentParser[Int]], Some(1))
    if cmd.done() then myProgram.add(arg1(), arg2())
end add

/** --- Some scenarios ----------------------------------------

> java add 2 3
2 + 3 = 5
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
error: missing argument for num
Usage: add num inc?
Adds two numbers
> java add 1 2 3
error: unused argument: 3
Usage: add num inc?
Adds two numbers
> java add --num 1 --incr 2
error: unknown argument name: --incr
Usage: add num inc?
Adds two numbers
> java add 1 true
error: invalid argument for inc: true
Usage: add num inc?
Adds two numbers
> java add true false
error: invalid argument for num: true
error: invalid argument for inc: false
Usage: add num inc?
Adds two numbers
> java add true false --foo 33
Error: invalid argument for num: true
Error: invalid argument for inc: false
Error: unknown argument name: --foo
Usage: add num inc?
Adds two numbers

*/
