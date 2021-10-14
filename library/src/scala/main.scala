/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import collection.mutable


/** An annotation that designates a main function
 */
class main extends scala.annotation.MainAnnotation:
  import main._

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  override def command(args: Array[String]): Command = new Command:

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
      () => throw new AssertionError("trying to get invalid argument")

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

    override def argGetter[T](argName: String)(using p: ArgumentParser[T]): () => T =
      argInfos += ((argName, ""))
      val idx = args.indexOf(s"--$argName")
      val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
      argOpt match
        case Some(arg) => convert(argName, arg, p)
        case None => error(s"missing argument for $argName")

    override def argGetter[T](argName: String, defaultValue: T)(using p: ArgumentParser[T]): () => T =
      argInfos += ((argName, "?"))
      val idx = args.indexOf(s"--$argName")
      val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
      argOpt match
        case Some(arg) => convert(argName, arg, p)
        case None => () => defaultValue

    override def argsGetter[T](argName: String)(using p: ArgumentParser[T]): () => Seq[T] =
      argInfos += ((argName, "*"))
      def remainingArgGetters(): List[() => T] = nextPositionalArg() match
        case Some(arg) => convert(argName, arg, p) :: remainingArgGetters()
        case None => Nil
      val getters = remainingArgGetters()
      () => getters.map(_())

    override def run(f: => MainResultType, progName: String, docComment: String): Unit =
      def usage(): Unit =
        println(s"Usage: $progName ${argInfos.map(_ + _).mkString(" ")}")

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

      if args.contains("--help") then
        usage()
        explain()
      else
        flagUnused()
        if errors.nonEmpty then
          for msg <- errors do println(s"Error: $msg")
          usage()
        else f match
          case ExitCode(n) => System.exit(n)
          case _ =>
    end run
  end command
end main

object main:
  case class ExitCode(val code: Int)
end main