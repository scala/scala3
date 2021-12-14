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

/**
  * An annotation that designates a main function.
  * @param maxLineLength the maximum number of characters to print on a single line when
  *                      displaying the help
  */
final class main(maxLineLength: Int) extends MainAnnotation:
  self =>
  import main._
  import MainAnnotation._

  /** An annotation that designates a main function. */
  def this() = this(120)

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  private enum ArgumentKind {
    case SimpleArgument, OptionalArgument, VarArgument
  }

  override def command(args: Array[String], commandName: String, docComment: String) =
    new Command[ArgumentParser, MainResultType]:
      private val argMarker = "--"
      private val shortArgMarker = "-"

      private var argCanonicalNames = new mutable.ArrayBuffer[String]
      private var argAlternativeNamess = new mutable.ArrayBuffer[Seq[String]]
      private var argShortNamess = new mutable.ArrayBuffer[Seq[Char]]
      private var argTypes = new mutable.ArrayBuffer[String]
      private var argDocOpts = new mutable.ArrayBuffer[Option[String]]
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
        val isFullName = arg.startsWith(argMarker)
        val isShortName = arg.startsWith(shortArgMarker) && arg.length == 2 && shortNameIsValid(arg(1))
        isFullName || isShortName

      private def nextPositionalArg(): Option[String] =
        while argIdx < args.length && isArgNameAt(argIdx) do argIdx += 2
        val result = argAt(argIdx)
        argIdx += 1
        result

      private def shortNameIsValid(shortName: Char): Boolean =
        // If you change this, remember to update the error message when an invalid short name is given
        shortName.isLetter

      private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
        p.fromStringOption(arg) match
          case Some(t) => () => t
          case None => error(s"invalid argument for $argName: $arg")

      private def allNamesWithMarkers(pos: Int): Seq[String] =
        val canonicalName = argMarker + argCanonicalNames(pos)
        val alternativeNames = argAlternativeNamess(pos).map(argMarker + _)
        val shortNames = argShortNamess(pos).map(shortArgMarker + _)
        canonicalName +: shortNames ++: alternativeNames

      private def argsUsage: Seq[String] =
        for (pos <- 0 until argCanonicalNames.length)
        yield {
          val namesPrint = allNamesWithMarkers(pos).mkString("[", " | ", "]")

          argKinds(pos) match {
            case ArgumentKind.SimpleArgument => s"$namesPrint <${argTypes(pos)}>"
            case ArgumentKind.OptionalArgument => s"[$namesPrint <${argTypes(pos)}>]"
            case ArgumentKind.VarArgument => s"[<${argTypes(pos)}> [<${argTypes(pos)}> [...]]]"
          }
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

      private def wrapArgumentUsages(argsUsage: Seq[String], maxLength: Int): Seq[String] = {
        def recurse(args: Seq[String], currentLine: String, acc: Vector[String]): Seq[String] =
          (args, currentLine) match {
            case (Nil, "") => acc
            case (Nil, l) => (acc :+ l)
            case (arg +: t, "") => recurse(t, arg, acc)
            case (arg +: t, l) if l.length + 1 + arg.length <= maxLength => recurse(t, s"$l $arg", acc)
            case (arg +: t, l) => recurse(t, arg, acc :+ l)
          }

        recurse(argsUsage, "", Vector()).toList
      }

      private inline def shiftLines(s: Seq[String], shift: Int): String = s.map(" " * shift + _).mkString("\n")

      private def usage(): Unit =
        val usageBeginning = s"Usage: $commandName "
        val argsOffset = usageBeginning.length
        val usages = wrapArgumentUsages(argsUsage, maxLineLength - argsOffset)

        println(usageBeginning + usages.mkString("\n" + " " * argsOffset))

      private def explain(): Unit =
        if (docComment.nonEmpty)
          println(wrapLongLine(docComment, maxLineLength).mkString("\n"))
        if (argCanonicalNames.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for (pos <- 0 until argCanonicalNames.length)
            val canonicalName = argMarker + argCanonicalNames(pos)
            val shortNames = argShortNamess(pos).map(shortArgMarker + _)
            val alternativeNames = argAlternativeNamess(pos).map(argMarker + _)
            val otherNames = (shortNames ++: alternativeNames) match {
              case Seq() => ""
              case names => names.mkString("(", ", ", ") ")
            }
            val argDoc = StringBuilder(" " * argNameShift)
            argDoc.append(s"$canonicalName $otherNames- ${argTypes(pos)}")

            argKinds(pos) match {
              case ArgumentKind.OptionalArgument => argDoc.append(" (optional)")
              case ArgumentKind.VarArgument => argDoc.append(" (vararg)")
              case _ =>
            }

            argDocOpts(pos).foreach(
              doc => if (doc.nonEmpty) {
                val shiftedDoc =
                  doc.split("\n").nn
                     .map(line => shiftLines(wrapLongLine(line.nn, maxLineLength - argDocShift), argDocShift))
                     .mkString("\n")
                argDoc.append("\n").append(shiftedDoc)
              }
            )

            println(argDoc)
        }

      private def indicesOfArg(argNames: Seq[String], shortArgNames: Seq[Char]): Seq[Int] =
        def allIndicesOf(s: String, from: Int): Seq[Int] =
          val i = args.indexOf(s, from)
          if i < 0 then Seq() else i +: allIndicesOf(s, i + 1)

        val indices = argNames.flatMap(name => allIndicesOf(s"$argMarker$name", 0))
        val indicesShort = shortArgNames.flatMap(shortName => allIndicesOf(s"$shortArgMarker$shortName", 0))
        (indices ++: indicesShort).filter(_ >= 0)

      private def getAlternativeNames(paramInfos: ParameterInfos[_]): Seq[String] =
        paramInfos.annotations.collect{ case annot: Name => annot.name }.filter(_.length > 0)

      private def getShortNames(paramInfos: ParameterInfos[_]): Seq[Char] =
        val (valid, invalid) =
          paramInfos.annotations.collect{ case annot: ShortName => annot.shortName }.partition(shortNameIsValid)
        if invalid.nonEmpty then
          throw IllegalArgumentException(s"invalid short names ${invalid.mkString(", ")} for parameter ${paramInfos.name}")
        valid

      private def checkNamesUnicity(namess: Seq[Seq[Any]]): Unit =
        val nameToIndex = namess.zipWithIndex.flatMap{ case (names, idx) => names.map(_ -> idx) }
        val nameToIndices = nameToIndex.groupMap(_._1)(_._2)
        for
          (name, indices) <- nameToIndices if indices.length > 1
        do
          val canonicalNamesAtIndices = indices.map(idx => argCanonicalNames(idx)).mkString(", ")
          throw AssertionError(s"$name is used for multiple parameters: $canonicalNamesAtIndices")

      private def checkNamesAreUnique(): Unit =
        val namess = argCanonicalNames.zip(argAlternativeNamess).map{ case (canon, alts) => canon +: alts }.toList
        checkNamesUnicity(namess)

      private def checkShortNamesAreUnique(): Unit =
        checkNamesUnicity(argShortNamess.toList)

      private def flagUnused(): Unit =
        nextPositionalArg() match
          case Some(arg) =>
            error(s"unused argument: $arg")
            flagUnused()
          case None =>
            val longNames = argCanonicalNames ++: argAlternativeNamess.flatten
            val shortNames = argShortNamess.flatten
            for
              arg <- args
            do
              val isInvalidArg =
                arg.length > argMarker.length
                && arg.startsWith(argMarker)
                && !longNames.contains(arg.drop(argMarker.length))
              val isInvalidShortArg =
                arg.length > shortArgMarker.length
                && arg.startsWith(shortArgMarker)
                && shortNameIsValid(arg(shortArgMarker.length))
                && !shortNames.contains(arg(shortArgMarker.length))
              if isInvalidArg || isInvalidShortArg then error(s"unknown argument name: $arg")

      private def registerArg(paramInfos: ParameterInfos[_], argKind: ArgumentKind): Unit =
        def checkNamesDuplicates(names: Seq[Any]): Unit =
          val occurrences = names.groupMapReduce(identity)(_ => 1)(_ + _)
          for (name, numOcc) <- occurrences if numOcc > 1
          do throw AssertionError(s"$name is declared multiple times for ${paramInfos.name}")

        argCanonicalNames += paramInfos.name
        argAlternativeNamess += getAlternativeNames(paramInfos)
        argShortNamess += getShortNames(paramInfos)
        argTypes += paramInfos.typeName
        argDocOpts += paramInfos.documentation
        argKinds += argKind

        // Check if names are used multiple times by the same param
        checkNamesDuplicates(argAlternativeNamess.last)
        checkNamesDuplicates(argShortNamess.last)

      override def argGetter[T](paramInfos: ParameterInfos[T])(using p: ArgumentParser[T]): () => T =
        val dvOpt = paramInfos.defaultValueGetterOpt
        registerArg(paramInfos, if dvOpt.nonEmpty then ArgumentKind.OptionalArgument else ArgumentKind.SimpleArgument)

        // registerArg placed all infos in the respective buffers
        val canonicalName = argCanonicalNames.last
        indicesOfArg(canonicalName +: argAlternativeNamess.last, argShortNamess.last) match {
          case s @ (Seq() | Seq(_)) =>
            val argOpt = s.headOption.map(idx => argAt(idx + 1)).getOrElse(nextPositionalArg())
            (argOpt, dvOpt) match {
              case (Some(arg), _) => convert(canonicalName, arg, p)
              case (None, Some(defaultValueGetter)) => defaultValueGetter
              case (None, None) => error(s"missing argument for ${paramInfos.name}")
            }
          case s =>
            val multValues = s.flatMap(idx => argAt(idx + 1))
            error(s"more than one value for $canonicalName: ${multValues.mkString(", ")}")
        }
      end argGetter

      override def varargGetter[T](paramInfos: ParameterInfos[T])(using p: ArgumentParser[T]): () => Seq[T] =
        registerArg(paramInfos, ArgumentKind.VarArgument)
        def remainingArgGetters(): List[() => T] = nextPositionalArg() match
          case Some(arg) => convert(paramInfos.name, arg, p) :: remainingArgGetters()
          case None => Nil
        val getters = remainingArgGetters()
        () => getters.map(_())

      override def run(f: => MainResultType): Unit =
        def checkAllConstraints(): Unit =
          flagUnused()
          checkNamesAreUnique()
          checkShortNamesAreUnique()

        if args.contains(s"${argMarker}help") then
          usage()
          println()
          explain()
        else
          checkAllConstraints()
          if errors.nonEmpty then
            for msg <- errors do println(s"Error: $msg")
            usage()
          else
            f
      end run
  end command
end main

object main:
  final class ShortName(val shortName: Char) extends MainAnnotation.ParameterAnnotation
  final class Name(val name: String) extends MainAnnotation.ParameterAnnotation
end main
