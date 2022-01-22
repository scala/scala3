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
  * The annotation that designates a main function.
  * Main functions are entry points for Scala programs. They can be called through a command line interface by using
  * the `scala` command, followed by their name and, optionally, their parameters.
  *
  * The parameters of a main function may have any type `T`, as long as there exists a
  * `given util.CommandLineParser.FromString[T]` in the scope. It will be used for parsing the string given as input
  * into the correct argument type.
  * These types already have parsers defined:
  * - String,
  * - Boolean,
  * - Byte, Short, Int, Long, Float, Double.
  *
  * The parameters of a main function may be passed either by position, or by name. Passing an argument positionaly
  * means that you give the arguments in the same order as the function's signature. Passing an argument by name means
  * that you give the argument right after giving its name. Considering the function
  * `@main def foo(i: Int, s: String)`, we may have arguments passed:
  * - by position: `scala foo 1 abc`,
  * - by name: `scala foo --i 1 --s abc` or `scala foo --s abc --i 1`.
  *
  * A mixture of both is also possible: `scala foo --s abc 1` is equivalent to all previous examples.
  *
  * Note that main function overloading is not currently supported, i.e. you cannot define two main methods that have
  * the same name in the same project.
  *
  * A special argument is used to display help regarding a main function: `--help`. If used as argument, the program
  * will display some useful information about the main function. This help directly uses the ScalaDoc comment
  * associated with the function, more precisely its description and the description of the parameters documented with
  * `@param`.
  *
  *
  * Parameters may be given annotations to add functionalities to the main function:
  * - `main.ShortName` adds a short name to a parameter. For example, if a parameter `node` has as short name `n`, it
  * may be addressed using either `--node` or `-n`,
  * - `main.Name` adds another name to a parameter. For example, if a parameter `node` has as alternative name
  * `otherNode`, it may be addressed using either `--node` or `--otherNode`.
  *
  * Here is an example of a main function with annotated parameters:
  * `@main def foo(@main.ShortName('x') number: Int, @main.Name("explanation") s: String)`. The following commands are
  * equivalent:
  * - `scala foo --number 1 --s abc`
  * - `scala foo -x 1 --s abc`
  * - `scala foo --number 1 --explanation abc`
  * - `scala foo -x 1 --explanation abc`
  *
  * @param maxLineLength the maximum number of characters to print on a single line when displaying the help
  */
final class main(maxLineLength: Int) extends MainAnnotation:
  import main._
  import MainAnnotation._

  /**
    * The annotation that designates a main function.
    * Main functions are entry points for Scala programs. They can be called through a command line interface by using
    * the `scala` command, followed by their name and, optionally, their parameters.
    *
    * The parameters of a main function may have any type `T`, as long as there exists a
    * `given util.CommandLineParser.FromString[T]` in the scope. It will be used for parsing the string given as input
    * into the correct argument type.
    * These types already have parsers defined:
    * - String,
    * - Boolean,
    * - Byte, Short, Int, Long, Float, Double.
    *
    * The parameters of a main function may be passed either by position, or by name. Passing an argument positionaly
    * means that you give the arguments in the same order as the function's signature. Passing an argument by name means
    * that you give the argument right after giving its name. Considering the function
    * `@main def foo(i: Int, s: String)`, we may have arguments passed:
    * - by position: `scala foo 1 abc`,
    * - by name: `scala foo --i 1 --s abc` or `scala foo --s abc --i 1`.
    *
    * A mixture of both is also possible: `scala foo --s abc 1` is equivalent to all previous examples.
    *
    * Note that main function overloading is not currently supported, i.e. you cannot define two main methods that have
    * the same name in the same project.
    *
    * A special argument is used to display help regarding a main function: `--help`. If used as argument, the program
    * will display some useful information about the main function. This help directly uses the ScalaDoc comment
    * associated with the function, more precisely its description and the description of the parameters documented with
    * `@param`.
    *
    *
    * Parameters may be given annotations to add functionalities to the main function:
    * - `main.ShortName` adds a short name to a parameter. For example, if a parameter `node` has as short name `n`, it
    * may be addressed using either `--node` or `-n`,
    * - `main.Name` adds another name to a parameter. For example, if a parameter `node` has as alternative name
    * `otherNode`, it may be addressed using either `--node` or `--otherNode`.
    *
    * Here is an example of a main function with annotated parameters:
    * `@main def foo(@main.ShortName('x') number: Int, @main.Name("explanation") s: String)`. The following commands are
    * equivalent:
    * - `scala foo --number 1 --s abc`
    * - `scala foo -x 1 --s abc`
    * - `scala foo --number 1 --explanation abc`
    * - `scala foo -x 1 --explanation abc`
    */
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
