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
  self =>
  import main._

  protected sealed abstract trait Argument {
    def name: String
    def typeName: String
    def doc: String

    def usage: String
  }
  protected class SimpleArgument(val name: String, val typeName: String, val doc: String) extends Argument:
    override def usage: String = s"[--$name] <$name>"
  protected class OptionalArgument[T](val name: String, val typeName: String, val doc: String, val defaultValue: T)
    extends Argument:
    override def usage: String = s"[[--$name] <$name>]"
  protected class VarArgument(val name: String, val typeName: String, val doc: String) extends Argument:
    override def usage: String = s"[<$name> [<$name> [...]]]"

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  /** Prints the main function's usage */
  def usage(commandName: String, args: Seq[Argument]): Unit =
    def wrappedArgumentUsages(argsUsage: List[String], maxLength: Int): List[String] =
      def recurse(args: List[String], currentLine: String, acc: Vector[String]): Vector[String] =
        (args, currentLine) match {
          case (Nil, "") => acc
          case (Nil, l) => (acc :+ l)
          case (arg :: t, "") => recurse(t, arg, acc)
          case (arg :: t, l) if l.length + 1 + arg.length <= maxLength => recurse(t, s"$l $arg", acc)
          case (arg :: t, l) => recurse(t, arg, acc :+ l)
        }

      recurse(argsUsage, "", Vector()).toList
    end wrappedArgumentUsages

    val maxLineLength = 120 // TODO as parameter? As global Dotty parameter?

    val usageBeginning = s"Usage: $commandName "
    val argsOffset = usageBeginning.length
    val argUsages = wrappedArgumentUsages(args.map(_.usage).toList, maxLineLength - argsOffset)

    println(usageBeginning + argUsages.mkString("\n" + " " * argsOffset))

  /** Prints an explanation about the function */
  def explain(commandName: String, commandDoc: String, args: Seq[Argument]): Unit =
    if (commandDoc.nonEmpty)
      println(commandDoc)
    if (args.nonEmpty) {
      val argNameShift = 2
      val argDocShift = argNameShift + 2

      println("Arguments:")
      for (arg <- args)
        val argDoc = StringBuilder(" " * argNameShift)
        argDoc.append(s"${arg.name} - ${arg.typeName}")

        arg match {
          case o: OptionalArgument[?] => argDoc.append(" (optional)")
          case _ =>
        }

        if (arg.doc.nonEmpty) {
          val shiftedDoc = arg.doc.split("\n").map(" " * argDocShift + _).mkString("\n")
          argDoc.append("\n").append(shiftedDoc)
        }

        println(argDoc)
    }

  /** Runs the command and handles its return value */
  def run(f: => MainResultType): Unit =
    f match
      case ExitCode(n) => System.exit(n)
      case _ =>

  override def command(args: Array[String], commandName: String, docComment: String): Command =
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

      private def indicesOfArg(argName: String): Seq[Int] =
        def allIndicesOf(s: String): Seq[Int] =
          def recurse(s: String, from: Int): Seq[Int] =
            val i = args.indexOf(s, from)
            if i < 0 then Seq() else i +: recurse(s, i + 1)

          recurse(s, 0)

        val indices = allIndicesOf(s"--$argName")
        indices.filter(_ >= 0)

      private def getArgGetter[T](argName: String, defaultGetter: => () => T)(using p: ArgumentParser[T]): () => T =
        indicesOfArg(argName) match {
          case s @ (Seq() | Seq(_)) =>
            val argOpt = s.headOption.map(idx => argAt(idx + 1)).getOrElse(nextPositionalArg())
            argOpt match {
              case Some(arg) => convert(argName, arg, p)
              case None => defaultGetter
            }
          case s =>
            val multValues = s.flatMap(idx => argAt(idx + 1))
            error(s"more than one value for $argName: ${multValues.mkString(", ")}")
        }

      override def argGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => T =
        argInfos += self.SimpleArgument(argName, argType, argDoc)
        getArgGetter(argName, error(s"missing argument for $argName"))

      override def argGetterDefault[T](argName: String, argType: String, argDoc: String, defaultValue: T)(using p: ArgumentParser[T]): () => T =
        argInfos += self.OptionalArgument(argName, argType, argDoc, defaultValue)
        getArgGetter(argName, () => defaultValue)

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