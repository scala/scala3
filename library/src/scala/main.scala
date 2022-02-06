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
  * `@main def foo(i: Int, str: String)`, we may have arguments passed:
  * - by position: `scala foo 1 abc`,
  * - by name: `scala foo -i 1 --str abc` or `scala foo --str abc -i 1`.
  *
  * A mixture of both is also possible: `scala foo --str abc 1` is equivalent to all previous examples.
  *
  * Note that main function overloading is not currently supported, i.e. you cannot define two main methods that have
  * the same name in the same project.
  *
  * A special argument is used to display help regarding a main function: `--help`. If used as argument, the program
  * will display some useful information about the main function. This help directly uses the ScalaDoc comment
  * associated with the function, more precisely its description and the description of the parameters documented with
  * `@param`.
  *
  * Parameters may be given annotations to add functionalities to the main function:
  * - `main.Alias` adds other names to a parameter. For example, if a parameter `node` has as aliases
  * `otherNode` and `n`, it may be addressed using `--node`, `--otherNode` or `-n`.
  *
  * Here is an example of a main function with annotated parameters:
  * `@main def foo(@main.Alias("x") number: Int, @main.Alias("explanation") s: String)`. The following commands are
  * equivalent:
  * - `scala foo --number 1 -s abc`
  * - `scala foo -x 1 -s abc`
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

  override def command(args: Array[String], commandName: String, documentation: String, parameterInfoss: ParameterInfos*) =
    new Command[ArgumentParser, MainResultType]:
      private enum ArgumentKind {
        case SimpleArgument, OptionalArgument, VarArgument
      }

      private val argMarker = "--"
      private val shortArgMarker = "-"

      /** A map from argument canonical name (the name of the parameter in the method definition) to parameter informations */
      private val nameToParameterInfos: Map[String, ParameterInfos] = parameterInfoss.map(infos => infos.name -> infos).toMap

      private val (positionalArgs, byNameArgs, invalidByNameArgs) = {
        val namesToCanonicalName: Map[String, String] = parameterInfoss.flatMap(
          infos =>
            var names = getAlternativeNames(infos)
            val canonicalName = infos.name
            if nameIsValid(canonicalName) then names = canonicalName +: names
            names.map(_ -> canonicalName)
        ).toMap
        val shortNamesToCanonicalName: Map[Char, String] = parameterInfoss.flatMap(
          infos =>
            var names = getShortNames(infos)
            val canonicalName = infos.name
            if shortNameIsValid(canonicalName) then names = canonicalName(0) +: names
            names.map(_ -> canonicalName)
        ).toMap

        def getCanonicalArgName(arg: String): Option[String] =
          if arg.startsWith(argMarker) && arg.length > argMarker.length then
            namesToCanonicalName.get(arg.drop(argMarker.length))
          else if arg.startsWith(shortArgMarker) && arg.length == shortArgMarker.length + 1 then
            shortNamesToCanonicalName.get(arg(shortArgMarker.length))
          else
            None

        def isArgName(arg: String): Boolean =
          val isFullName = arg.startsWith(argMarker)
          val isShortName = arg.startsWith(shortArgMarker) && arg.length == shortArgMarker.length + 1 && shortNameIsValid(arg(shortArgMarker.length))
          isFullName || isShortName

        def recurse(remainingArgs: Seq[String], pa: mutable.Queue[String], bna: Seq[(String, String)], ia: Seq[String]): (mutable.Queue[String], Seq[(String, String)], Seq[String]) =
          remainingArgs match {
            case Seq() =>
              (pa, bna, ia)
            case argName +: argValue +: rest if isArgName(argName) =>
              getCanonicalArgName(argName) match {
                case Some(canonicalName) => recurse(rest, pa, bna :+ (canonicalName -> argValue), ia)
                case None => recurse(rest, pa, bna, ia :+ argName)
              }
            case arg +: rest =>
              recurse(rest, pa :+ arg, bna, ia)
          }

        val (pa, bna, ia) = recurse(args.toSeq, mutable.Queue.empty, Vector(), Vector())
        val nameToArgValues: Map[String, Seq[String]] = if bna.isEmpty then Map.empty else bna.groupMapReduce(_._1)(p => List(p._2))(_ ++ _)
        (pa, nameToArgValues, ia)
      }

      /** The kind of the arguments. Used to display help about the main method. */
      private val argKinds = new mutable.ArrayBuffer[ArgumentKind]

      /** A buffer for all errors */
      private val errors = new mutable.ArrayBuffer[String]

      /** Issue an error, and return an uncallable getter */
      private def error(msg: String): () => Nothing =
        errors += msg
        () => throw new AssertionError("trying to get invalid argument")

      private inline def nameIsValid(name: String): Boolean =
        name.length > 1 // TODO add more checks for illegal characters

      private inline def shortNameIsValid(name: String): Boolean =
        name.length == 1 && shortNameIsValid(name(0))

      private inline def shortNameIsValid(shortName: Char): Boolean =
        ('A' <= shortName && shortName <= 'Z') || ('a' <= shortName && shortName <= 'z')

      private def getNameWithMarker(name: String | Char): String = name match {
        case c: Char => shortArgMarker + c
        case s: String if shortNameIsValid(s) => shortArgMarker + s
        case s => argMarker + s
      }

      private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
        p.fromStringOption(arg) match
          case Some(t) => () => t
          case None => error(s"invalid argument for $argName: $arg")

      private def usage(): Unit =
        def argsUsage: Seq[String] =
          for ((infos, kind) <- parameterInfoss.zip(argKinds))
          yield {
            val canonicalName = getNameWithMarker(infos.name)
            val shortNames = getShortNames(infos).map(getNameWithMarker)
            val alternativeNames = getAlternativeNames(infos).map(getNameWithMarker)
            val namesPrint = (canonicalName +: alternativeNames ++: shortNames).mkString("[", " | ", "]")

            kind match {
              case ArgumentKind.SimpleArgument => s"$namesPrint <${infos.typeName}>"
              case ArgumentKind.OptionalArgument => s"[$namesPrint <${infos.typeName}>]"
              case ArgumentKind.VarArgument => s"[<${infos.typeName}> [<${infos.typeName}> [...]]]"
            }
          }

        def wrapArgumentUsages(argsUsage: Seq[String], maxLength: Int): Seq[String] = {
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

        val usageBeginning = s"Usage: $commandName "
        val argsOffset = usageBeginning.length
        val usages = wrapArgumentUsages(argsUsage, maxLineLength - argsOffset)

        println(usageBeginning + usages.mkString("\n" + " " * argsOffset))
      end usage

      private def explain(): Unit =
        inline def shiftLines(s: Seq[String], shift: Int): String = s.map(" " * shift + _).mkString("\n")

        def wrapLongLine(line: String, maxLength: Int): List[String] = {
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

        if (documentation.nonEmpty)
          println(wrapLongLine(documentation, maxLineLength).mkString("\n"))
        if (nameToParameterInfos.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for ((infos, kind) <- parameterInfoss.zip(argKinds))
            val canonicalName = getNameWithMarker(infos.name)
            val shortNames = getShortNames(infos).map(getNameWithMarker)
            val alternativeNames = getAlternativeNames(infos).map(getNameWithMarker)
            val otherNames = (alternativeNames ++: shortNames) match {
              case Seq() => ""
              case names => names.mkString("(", ", ", ") ")
            }
            val argDoc = StringBuilder(" " * argNameShift)
            argDoc.append(s"$canonicalName $otherNames- ${infos.typeName}")

            kind match {
              case ArgumentKind.OptionalArgument => argDoc.append(" (optional)")
              case ArgumentKind.VarArgument => argDoc.append(" (vararg)")
              case _ =>
            }

            infos.documentation.foreach(
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
      end explain

      private def getAliases(paramInfos: ParameterInfos): Seq[String] =
        paramInfos.annotations.collect{ case a: Alias => a }.flatMap(_.aliases)

      private def getAlternativeNames(paramInfos: ParameterInfos): Seq[String] =
        getAliases(paramInfos).filter(nameIsValid(_))

      private def getShortNames(paramInfos: ParameterInfos): Seq[Char] =
        getAliases(paramInfos).filter(shortNameIsValid(_)).map(_(0))

      private def getInvalidNames(paramInfos: ParameterInfos): Seq[String | Char] =
        getAliases(paramInfos).filter(name => !nameIsValid(name) && !shortNameIsValid(name))

      private def checkAliasesValidity(): Unit =
        val problematicNames = nameToParameterInfos.toList.flatMap((_, infos) => getInvalidNames(infos))
        if problematicNames.length > 0 then throw IllegalArgumentException(s"The following aliases are invalid: ${problematicNames.mkString(", ")}")

      private def checkAliasesUnicity(): Unit =
        val nameAndCanonicalName = nameToParameterInfos.toList.flatMap {
          case (canonicalName, infos) => (canonicalName +: getAlternativeNames(infos) ++: getShortNames(infos)).map(_ -> canonicalName)
        }
        val nameToCanonicalNames = nameAndCanonicalName.groupMap(_._1)(_._2)

        for (name, canonicalNames) <- nameToCanonicalNames if canonicalNames.length > 1
        do throw IllegalArgumentException(s"$name is used for multiple parameters: ${canonicalNames.mkString(", ")}")

      override def argGetter[T](name: String, optDefaultGetter: Option[() => T])(using p: ArgumentParser[T]): () => T =
        argKinds += (if optDefaultGetter.nonEmpty then ArgumentKind.OptionalArgument else ArgumentKind.SimpleArgument)
        val parameterInfos = nameToParameterInfos(name)

        byNameArgs.get(name) match {
          case Some(Nil) =>
            throw AssertionError(s"$name present in byNameArgs, but it has no argument value")
          case Some(argValues) =>
            if argValues.length > 1 then
              // Do not accept multiple values
              // Remove this test to take last given argument
              error(s"more than one value for $name: ${argValues.mkString(", ")}")
            else
              convert(name, argValues.last, p)
          case None =>
            if positionalArgs.length > 0 then
              convert(name, positionalArgs.dequeue, p)
            else if optDefaultGetter.nonEmpty then
              optDefaultGetter.get
            else
              error(s"missing argument for $name")
        }
      end argGetter

      override def varargGetter[T](name: String)(using p: ArgumentParser[T]): () => Seq[T] =
        argKinds += ArgumentKind.VarArgument

        val byNameGetters = byNameArgs.getOrElse(name, Seq()).map(arg => convert(name, arg, p))
        val positionalGetters = positionalArgs.removeAll.map(arg => convert(name, arg, p))
        // First take arguments passed by name, then those passed by position
        () => (byNameGetters ++ positionalGetters).map(_())

      override def run(f: => MainResultType): Unit =
        checkAliasesUnicity()
        checkAliasesValidity()
        for (remainingArg <- positionalArgs) error(s"unused argument: $remainingArg")
        for (invalidArg <- invalidByNameArgs) error(s"unknown argument name: $invalidArg")

        if args.contains(s"${argMarker}help") then
          usage()
          println()
          explain()
        else if errors.nonEmpty then
          for msg <- errors do println(s"Error: $msg")
          usage()
        else
          f
      end run
  end command
end main

object main:
  final class Alias(val aliases: String*) extends MainAnnotation.ParameterAnnotation
end main
