/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import collection.mutable
import annotation._

/** An annotation that designates a main function
 */
final class main(maxLineLength: Int) extends MainAnnotation:
  self =>
  import main._
  import MainAnnotation._

  def this() = this(120)

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  private enum ArgumentKind {
    case SimpleArgument, OptionalArgument, VarArgument
  }

  override def command(args: Array[String], commandName: String, docComment: String) =
    new Command[ArgumentParser, MainResultType]:
      private var argNames = new mutable.ArrayBuffer[String]
      private var argShortNames = new mutable.ArrayBuffer[Option[Char]]
      private var argTypes = new mutable.ArrayBuffer[String]
      private var argDocs = new mutable.ArrayBuffer[String]
      private var argKinds = new mutable.ArrayBuffer[ArgumentKind]

      /** A buffer for all errors */
      private var errors = new mutable.ArrayBuffer[String]

      /** Issue an error, and return an uncallable getter */
      private def error(msg: String): () => Nothing =
        errors += msg
        () => throw new AssertionError("trying to get invalid argument")

      /** The next argument index */
      private var argIdx: Int = 0

      private def argAt(idx: Int): Option[String] =
        if idx < args.length then Some(args(idx)) else None

      private def isArgNameAt(idx: Int): Boolean =
        val arg = args(argIdx)
        val isFullName = arg.startsWith("--")
        val isShortName = arg.startsWith("-") && arg.length == 2 && shortNameIsValid(arg(1))

        isFullName || isShortName

      private def nextPositionalArg(): Option[String] =
        while argIdx < args.length && isArgNameAt(argIdx) do argIdx += 2
        val result = argAt(argIdx)
        argIdx += 1
        result

      private def shortNameIsValid(shortName: Char): Boolean =
        shortName == 0 || shortName.isLetter

      private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
        p.fromStringOption(arg) match
          case Some(t) => () => t
          case None => error(s"invalid argument for $argName: $arg")

      private def argUsage(pos: Int): String =
        val name = argNames(pos)
        val namePrint = argShortNames(pos).map(short => s"[-$short | --$name]").getOrElse(s"[--$name]")

        argKinds(pos) match {
          case ArgumentKind.SimpleArgument => s"$namePrint <${argTypes(pos)}>"
          case ArgumentKind.OptionalArgument => s"[$namePrint <${argTypes(pos)}>]"
          case ArgumentKind.VarArgument => s"[<${argTypes(pos)}> [<${argTypes(pos)}> [...]]]"
        }

      private def wrapLongLine(line: String, maxLength: Int): List[String] = {
        def recurse(s: String, acc: Vector[String]): Seq[String] =
          val lastSpace = s.trim.nn.lastIndexOf(' ', maxLength)
          if ((s.length <= maxLength) || (lastSpace < 0))
            acc :+ s
          else {
            val (shortLine, rest) = s.splitAt(lastSpace)
            recurse(rest.trim.nn, acc :+ shortLine)
          }

        recurse(line, Vector()).toList
      }

      private def wrapArgumentUsages(argsUsage: List[String], maxLength: Int): List[String] = {
        def recurse(args: List[String], currentLine: String, acc: Vector[String]): Seq[String] =
          (args, currentLine) match {
            case (Nil, "") => acc
            case (Nil, l) => (acc :+ l)
            case (arg :: t, "") => recurse(t, arg, acc)
            case (arg :: t, l) if l.length + 1 + arg.length <= maxLength => recurse(t, s"$l $arg", acc)
            case (arg :: t, l) => recurse(t, arg, acc :+ l)
          }

        recurse(argsUsage, "", Vector()).toList
      }

      private inline def shiftLines(s: Seq[String], shift: Int): String = s.map(" " * shift + _).mkString("\n")

      private def usage(): Unit =
        val usageBeginning = s"Usage: $commandName "
        val argsOffset = usageBeginning.length
        val argUsages = wrapArgumentUsages((0 until argNames.length).map(argUsage).toList, maxLineLength - argsOffset)

        println(usageBeginning + argUsages.mkString("\n" + " " * argsOffset))

      private def explain(): Unit =
        if (docComment.nonEmpty)
          println(wrapLongLine(docComment, maxLineLength).mkString("\n"))
        if (argNames.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for (pos <- 0 until argNames.length)
            val argDoc = StringBuilder(" " * argNameShift)
            argDoc.append(s"${argNames(pos)} - ${argTypes(pos)}")

            argKinds(pos) match {
              case ArgumentKind.OptionalArgument => argDoc.append(" (optional)")
              case ArgumentKind.VarArgument => argDoc.append(" (vararg)")
              case _ =>
            }

            if (argDocs(pos).nonEmpty) {
              val shiftedDoc =
                argDocs(pos).split("\n").nn
                            .map(line => shiftLines(wrapLongLine(line.nn, maxLineLength - argDocShift), argDocShift))
                            .mkString("\n")
              argDoc.append("\n").append(shiftedDoc)
            }

            println(argDoc)
        }

      private def indicesOfArg(argName: String, shortArgName: Option[Char]): Seq[Int] =
        def allIndicesOf(s: String): Seq[Int] =
          def recurse(s: String, from: Int): Seq[Int] =
            val i = args.indexOf(s, from)
            if i < 0 then Seq() else i +: recurse(s, i + 1)

          recurse(s, 0)

        val indices = allIndicesOf(s"--$argName")
        val indicesShort = shortArgName.map(shortName => allIndicesOf(s"-$shortName")).getOrElse(Seq())
        (indices ++: indicesShort).filter(_ >= 0)

      private def getArgGetter[T](paramInfos: ParameterInfos[_], getDefaultGetter: () => () => T)(using p: ArgumentParser[T]): () => T =
        val argName = getEffectiveName(paramInfos)
        indicesOfArg(argName, getShortName(paramInfos)) match {
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

      private def getAnnotationData[T](paramInfos: ParameterInfos[_], extractor: Arg => T): Option[T] =
        paramInfos.annotation match {
          case Some(annot: Arg) => Some(extractor(annot))
          case _ => None
        }

      private inline def getEffectiveName(paramInfos: ParameterInfos[_]): String =
        getAnnotationData(paramInfos, _.name).filter(_.length > 0).getOrElse(paramInfos.name)

      private inline def getShortName(paramInfos: ParameterInfos[_]): Option[Char] =
        getAnnotationData(paramInfos, _.shortName).filterNot(_ == 0)

      private def registerArg(paramInfos: ParameterInfos[_], argKind: ArgumentKind): Unit =
        argNames += getEffectiveName(paramInfos)
        argTypes += paramInfos.typeName
        argDocs += paramInfos.documentation.getOrElse("")
        argKinds += argKind

        val shortName = getShortName(paramInfos)
        if shortName.exists(c => !shortNameIsValid(c)) then throw IllegalArgumentException(s"Invalid short name: -${shortName.get}")
        argShortNames += shortName

      override def argGetter[T](paramInfos: ParameterInfos[T])(using p: ArgumentParser[T]): () => T =
        val name = getEffectiveName(paramInfos)
        val (defaultGetter, argumentKind) = paramInfos.defaultValue match {
          case Some(value) => (() => () => value, ArgumentKind.OptionalArgument)
          case None => (() => error(s"missing argument for $name"), ArgumentKind.SimpleArgument)
        }
        registerArg(paramInfos, argumentKind)
        getArgGetter(paramInfos, defaultGetter)

      override def varargGetter[T](paramInfos: ParameterInfos[T])(using p: ArgumentParser[T]): () => Seq[T] =
        registerArg(paramInfos, ArgumentKind.VarArgument)
        def remainingArgGetters(): List[() => T] = nextPositionalArg() match
          case Some(arg) => convert(getEffectiveName(paramInfos), arg, p) :: remainingArgGetters()
          case None => Nil
        val getters = remainingArgGetters()
        () => getters.map(_())

      override def run(f: => MainResultType): Unit =
        def checkShortNamesUnique(): Unit =
          val shortNameToIndices = argShortNames.collect{ case Some(short) => short }.zipWithIndex.groupBy(_._1).view.mapValues(_.map(_._2))
          for ((shortName, indices) <- shortNameToIndices if indices.length > 1)
            error(s"$shortName is used as short name for multiple parameters: ${indices.map(idx => argNames(idx)).mkString(", ")}")

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
          checkShortNamesUnique()
          if errors.nonEmpty then
            for msg <- errors do println(s"Error: $msg")
            usage()
          else
            f
      end run
  end command
end main

object main:
  final class Arg(val name: String = "", val shortName: Char = 0) extends MainAnnotation.ParameterAnnotation
end main
