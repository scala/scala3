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

  protected sealed abstract trait Argument {
    def name: String
    def typeName: String
    def doc: String
  }
  protected case class SimpleArgument(name: String, typeName: String, doc: String) extends Argument
  protected case class OptionalArgument[T](name: String, typeName: String, doc: String, val defaultValue: T) extends Argument
  protected case class VarArgument(name: String, typeName: String, doc: String) extends Argument

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  /** Prints the main function's usage */
  def usage(commandName: String, args: Seq[Argument]): Unit =
    val argInfos = args map (
      _ match {
        case SimpleArgument(name, _, _) => name
        case OptionalArgument(name, _, _, _) => s"$name?"
        case VarArgument(name, _, _) => s"$name*"
      }
    )
    println(s"Usage: $commandName ${argInfos.mkString(" ")}")

  /** Prints an explanation about the function */
  def explain(commandName: String, commandDoc: String, args: Seq[Argument]): Unit =
    if (commandDoc.nonEmpty)
      println(commandDoc)
    if (args.nonEmpty) {
      println("Arguments:")
      for (arg <- args)
        val argDoc = if arg.doc.isEmpty then "" else s" - ${arg.doc}"
        println(s"\t${arg.name}, ${arg.typeName}${argDoc}")
    }

  /** Runs the command and handles its return value */
  def run(f: => MainResultType): Unit =
    f match
      case ExitCode(n) => System.exit(n)
      case _ =>

  override def command(args: Array[String], commandName: String, docComment: String): Command =
    val self = this
    new Command(commandName, docComment):
      /** A buffer of demanded arguments */
      private var argInfos = new mutable.ListBuffer[self.Argument]

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

      private def usage(): Unit =
        self.usage(this.commandName, argInfos.toSeq)

      private def explain(): Unit =
        self.explain(this.commandName, this.docComment, argInfos.toSeq)

      override def argGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => T =
        argInfos += self.SimpleArgument(argName, argType, argDoc)
        val idx = args.indexOf(s"--$argName")
        val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
        argOpt match
          case Some(arg) => convert(argName, arg, p)
          case None => error(s"missing argument for $argName")

      override def argGetterDefault[T](argName: String, argType: String, argDoc: String, defaultValue: T)(using p: ArgumentParser[T]): () => T =
        argInfos += self.OptionalArgument(argName, argType, argDoc, defaultValue)
        val idx = args.indexOf(s"--$argName")
        val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
        argOpt match
          case Some(arg) => convert(argName, arg, p)
          case None => () => defaultValue

      override def argsGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => Seq[T] =
        argInfos += self.VarArgument(argName, argType, argDoc)
        def remainingArgGetters(): List[() => T] = nextPositionalArg() match
          case Some(arg) => convert(argName, arg, p) :: remainingArgGetters()
          case None => Nil
        val getters = remainingArgGetters()
        () => getters.map(_())

      override def run(f: => MainResultType): Unit =
        def flagUnused(): Unit = nextPositionalArg() match
          case Some(arg) =>
            error(s"unused argument: $arg")
            flagUnused()
          case None =>
            for
              arg <- args
              if arg.startsWith("--") && !argInfos.map(_.name).contains(arg.drop(2))
            do
              error(s"unknown argument name: $arg")
        end flagUnused

        if args.contains("--help") then
          usage()
          println()
          explain()
        else
          flagUnused()
          if errors.nonEmpty then
            for msg <- errors do println(s"Error: $msg")
            usage()
          else
            self.run(f)
      end run
  end command
end main

object main:
  case class ExitCode(val code: Int)
end main