/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import collection.mutable
import annotation.MainAnnotation

/** An annotation that designates a main function
 */
final class main extends scala.annotation.MainAnnotation:
  self =>
  import main._

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  private enum ArgumentKind {
    case SimpleArgument, OptionalArgument, VarArgument
  }

  override def command(args: Array[String], commandName: String, docComment: String) =
    new MainAnnotation.Command[ArgumentParser, MainResultType]:
      private var argNames = new mutable.ListBuffer[String]
      private var argTypes = new mutable.ListBuffer[String]
      private var argDocs = new mutable.ListBuffer[String]
      private var argKinds = new mutable.ListBuffer[ArgumentKind]

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

      private def argUsage(pos: Int): String =
        val name = argNames(pos)

        argKinds(pos) match {
          case ArgumentKind.SimpleArgument => s"[--$name] <$name>"
          case ArgumentKind.OptionalArgument => s"[[--$name] <$name>]"
          case ArgumentKind.VarArgument => s"[<$name> [<$name> [...]]]"
        }

      private def usage(): Unit =
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
        val argUsages = wrappedArgumentUsages((0 until argNames.length).map(argUsage).toList, maxLineLength - argsOffset)

        println(usageBeginning + argUsages.mkString("\n" + " " * argsOffset))

      private def explain(): Unit =
        if (docComment.nonEmpty)
          println(docComment)
        if (argNames.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for (pos <- 0 until argNames.length)
            val argDoc = StringBuilder(" " * argNameShift)
            argDoc.append(s"${argNames(pos)} - ${argTypes(pos)}")

            argKinds(pos) match {
              case ArgumentKind.OptionalArgument => argDoc.append(" (optional)")
              case _ =>
            }

            if (argDocs(pos).nonEmpty) {
              val shiftedDoc = argDocs(pos).split("\n").nn.map(" " * argDocShift + _).mkString("\n")
              argDoc.append("\n").append(shiftedDoc)
            }

            println(argDoc)
        }

      private def indicesOfArg(argName: String): Seq[Int] =
        def allIndicesOf(s: String): Seq[Int] =
          def recurse(s: String, from: Int): Seq[Int] =
            val i = args.indexOf(s, from)
            if i < 0 then Seq() else i +: recurse(s, i + 1)

          recurse(s, 0)

        val indices = allIndicesOf(s"--$argName")
        indices.filter(_ >= 0)

      private def getArgGetter[T](argName: String, getDefaultGetter: () => () => T)(using p: ArgumentParser[T]): () => T =
        indicesOfArg(argName) match {
          case s @ (Seq() | Seq(_)) =>
            val argOpt = s.headOption.map(idx => argAt(idx + 1)).getOrElse(nextPositionalArg())
            argOpt match {
              case Some(arg) => convert(argName, arg, p)
              case None => getDefaultGetter()
            }
          case s =>
            val multValues = s.flatMap(idx => argAt(idx + 1))
            error(s"more than one value for $argName: ${multValues.mkString(", ")}")
        }

      private def registerArg(argName: String, argType: String, argDoc: String, argKind: ArgumentKind): Unit =
        argNames += argName
        argTypes += argType
        argDocs += argDoc
        argKinds += argKind

      override def argGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => T =
        registerArg(argName, argType, argDoc, ArgumentKind.SimpleArgument)
        getArgGetter(argName, () => error(s"missing argument for $argName"))

      override def argGetterDefault[T](argName: String, argType: String, argDoc: String, defaultValue: => T)(using p: ArgumentParser[T]): () => T =
        registerArg(argName, argType, argDoc, ArgumentKind.OptionalArgument)
        getArgGetter(argName, () => () => defaultValue)

      override def argsGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => Seq[T] =
        registerArg(argName, argType, argDoc, ArgumentKind.VarArgument)
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
              if arg.startsWith("--") && !argNames.contains(arg.drop(2))
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
          else f match
            case ExitCode(n) => sys.exit(n)
            case () =>
            case res => println(res)
      end run
  end command
end main

object main:
  case class ExitCode(code: Int)
end main