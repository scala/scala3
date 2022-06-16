/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

import scala.collection.mutable
import scala.util.CommandLineParser.FromString
import scala.annotation.meta.param

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
 * The parameters of a main function may be passed either by position, or by name. Passing an argument positionally
 * means that you give the arguments in the same order as the function's signature. Passing an argument by name means
 * that you give the argument right after giving its name. Considering the function
 * `@newMain def foo(i: Int, str: String)`, we may have arguments passed:
 * - by position: `scala foo 1 abc`,
 * - by name: `scala foo -i 1 --str abc` or `scala foo --str abc -i 1`.
 *
 * A mixture of both is also possible: `scala foo --str abc 1` is equivalent to all previous examples.
 *
 * Note that main function overloading is not currently supported, i.e. you cannot define two main methods that have
 * the same name in the same project.
 *
 * Special arguments are used to display help regarding a main function: `--help` and `-h`. If used as argument, the program
 * will display some useful information about the main function. This help directly uses the ScalaDoc comment
 * associated with the function, more precisely its description and the description of the parameters documented with
 * `@param`. Note that if a parameter is named `help` or `h`, or if one of the parameters has as alias one of those names,
 * the help displaying will be disabled for that argument.
 * For example, for `@newMain def foo(help: Boolean)`, `scala foo -h` will display the help, but `scala foo --help` will fail,
 * as it will expect a Boolean value after `--help`.
 *
 * Parameters may be given annotations to add functionalities to the main function:
 * - `main.alias` adds other names to a parameter. For example, if a parameter `node` has as aliases
 * `otherNode` and `n`, it may be addressed using `--node`, `--otherNode` or `-n`.
 *
 * Here is an example of a main function with annotated parameters:
 * `@newMain def foo(@newMain.alias("x") number: Int, @newMain.alias("explanation") s: String)`. The following commands are
 * equivalent:
 * - `scala foo --number 1 -s abc`
 * - `scala foo -x 1 -s abc`
 * - `scala foo --number 1 --explanation abc`
 * - `scala foo -x 1 --explanation abc`
 *
 * Boolean parameters are considered flags that do not require the "true" or "false" value to be passed.
 * For example, `@newMain def foo(i: Boolean)` can be called as `foo` (where `i=false`) or `foo -i` (where `i=true`).
 *
 * The special `--` marker can be used to indicate that all following arguments are passed verbatim as positional parameters.
 * For example, `@newMain def foo(args: String*)` can be called as `scala foo a b -- -c -d` which implies that `args=Seq("a", "b", "-c", "-d")`.
 */
@experimental
final class newMain extends MainAnnotation[FromString, Any]:
  import newMain._
  import MainAnnotation._

  private val longArgRegex = "--[a-zA-Z][a-zA-Z0-9]+".r
  private val shortArgRegex = "-[a-zA-Z]".r
  // TODO: what should be considered as an invalid argument?
  //       Consider argument `-3.14`, `--i`, `-name`
  private val illFormedName = "--[a-zA-Z]|-[a-zA-Z][a-zA-Z0-9]+".r
  /** After this marker, all arguments are positional */
  private inline val positionArgsMarker = "--"

  extension (param: Parameter)
    private def aliasNames: Seq[String] =
      param.annotations.collect{ case alias: alias => getNameWithMarker(alias.name) }
    private def isFlag: Boolean =
      param.typeName == "scala.Boolean"

  private def getNameWithMarker(name: String): String =
    if name.length > 1 then s"--$name"
    else if name.length == 1 then s"-$name"
    else assert(false, "invalid name")

  def command(info: Info, args: Seq[String]): Option[Seq[String]] =
    val names = Names(info)
    if Help.shouldPrintDefaultHelp(names, args) then
      Help.printUsage(info)
      Help.printExplain(info)
      None
    else
      preProcessArgs(info, names, args).orElse {
        Help.printUsage(info)
        None
      }
  end command

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using p: FromString[T]): () => T = {
    if arg.nonEmpty then parse[T](param, arg)
    else
      assert(param.hasDefault)

      defaultArgument.get
  }

  def varargGetter[T](param: Parameter, args: Seq[String])(using p: FromString[T]): () => Seq[T] = {
    val getters = args.map(arg => parse[T](param, arg))
    () => getters.map(_())
  }

  def run(execProgram: () => Any): Unit =
    if !hasParseErrors then execProgram()

  private def preProcessArgs(info: Info, names: Names, args: Seq[String]): Option[Seq[String]] =
    var hasError: Boolean = false
    def error(msg: String): Unit = {
      hasError = true
      println(s"Error: $msg")
    }

    val (positionalArgs, byNameArgsMap) =
      val positionalArgs = List.newBuilder[String]
      val byNameArgs = List.newBuilder[(String, String)]
      val flagsAdded = mutable.Set.empty[String]
      // TODO: once we settle on a spec, we should implement this in a more elegant way
      var i = 0
      while i < args.length do
        args(i) match
          case name @ (longArgRegex() | shortArgRegex()) =>
            if names.isFlagName(name) then
              val canonicalName = names.canonicalName(name).get
              flagsAdded += canonicalName
              byNameArgs += ((canonicalName, "true"))
            else if i == args.length - 1 then // last argument -x ot --xyz
              error(s"missing argument for ${name}")
            else args(i + 1) match
              case longArgRegex() | shortArgRegex() | `positionArgsMarker` =>
                error(s"missing argument for ${name}")
              case value =>
                names.canonicalName(name) match
                  case Some(canonicalName) =>
                    byNameArgs += ((canonicalName, value))
                  case None =>
                    error(s"unknown argument name: $name")
                i += 1 // consume `value`
          case name @ illFormedName() =>
            error(s"ill-formed argument name: $name")
          case `positionArgsMarker` =>
            i += 1 // skip `--`
            // all args after `--` are positional args
            while i < args.length do
              positionalArgs += args(i)
              i += 1
          case value =>
            positionalArgs += value
        i += 1
      end while

      // Add "false" for all flags not present in the arguments
      for
        param <- info.parameters
        if param.isFlag
        name = getNameWithMarker(param.name)
        if !flagsAdded.contains(name)
      do
        byNameArgs += ((name, "false"))

      (positionalArgs.result(), byNameArgs.result().groupMap(_._1)(_._2))

    // List of arguments in the order they should be passed to the main function
    val orderedArgs: List[String] =
      def rec(params: List[Parameter], acc: List[String], remainingArgs: List[String]): List[String] =
        params match
          case Nil =>
            for (remainingArg <- remainingArgs) error(s"unused argument: $remainingArg")
            acc.reverse
          case param :: tailParams =>
            if param.isVarargs then // also last arguments
              byNameArgsMap.get(param.name) match
                case Some(byNameVarargs) => acc.reverse ::: byNameVarargs.toList ::: remainingArgs
                case None => acc.reverse ::: remainingArgs
            else byNameArgsMap.get(getNameWithMarker(param.name)) match
              case Some(argValues) =>
                assert(argValues.nonEmpty, s"${param.name} present in byNameArgsMap, but it has no argument value")
                if argValues.length > 1 then
                  error(s"more than one value for ${param.name}: ${argValues.mkString(", ")}")
                rec(tailParams, argValues.last :: acc, remainingArgs)

              case None =>
                remainingArgs match
                  case arg :: rest =>
                    rec(tailParams, arg :: acc, rest)
                  case Nil =>
                    if !param.hasDefault then
                      error(s"missing argument for ${param.name}")
                    rec(tailParams, "" :: acc, Nil)
      rec(info.parameters.toList, Nil, positionalArgs)

    if hasError then None
    else Some(orderedArgs)
  end preProcessArgs

  private var hasParseErrors: Boolean = false

  private def parse[T](param: Parameter, arg: String)(using p: FromString[T]): () => T =
    p.fromStringOption(arg) match
      case Some(t) =>
        () => t
      case None =>
        /** Issue an error, and return an uncallable getter */
        println(s"Error: could not parse argument for `${param.name}` of type ${param.typeName.split('.').last}: $arg")
        hasParseErrors = true
        () => throw new AssertionError("trying to get invalid argument")

  @experimental // MiMa does not check scope inherited @experimental
  private object Help:

    /** The name of the special argument to display the method's help.
     *  If one of the method's parameters is called the same, will be ignored.
     */
    private inline val helpArg = "--help"

    /** The short name of the special argument to display the method's help.
     *  If one of the method's parameters uses the same short name, will be ignored.
     */
    private inline val shortHelpArg = "-h"

    private inline val maxUsageLineLength = 120

    def printUsage(info: Info): Unit =
      def argsUsage: Seq[String] =
        for (param <- info.parameters)
        yield {
          val canonicalName = getNameWithMarker(param.name)
          val namesPrint = (canonicalName +: param.aliasNames).mkString("[", " | ", "]")
          val shortTypeName = param.typeName.split('.').last
          if param.isVarargs then s"[<$shortTypeName> [<$shortTypeName> [...]]]"
          else if param.hasDefault then s"[$namesPrint <$shortTypeName>]"
          else if param.isFlag then s"$namesPrint"
          else s"$namesPrint <$shortTypeName>"
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

      val printUsageBeginning = s"Usage: ${info.name} "
      val argsOffset = printUsageBeginning.length
      val printUsages = wrapArgumentUsages(argsUsage, maxUsageLineLength - argsOffset)

      println(printUsageBeginning + printUsages.mkString("\n" + " " * argsOffset))
    end printUsage

    def printExplain(info: Info): Unit =
      def shiftLines(s: Seq[String], shift: Int): String = s.map(" " * shift + _).mkString("\n")

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

      println()

      if (info.documentation.nonEmpty)
        println(wrapLongLine(info.documentation, maxUsageLineLength).mkString("\n"))
      if (info.parameters.nonEmpty) {
        val argNameShift = 2
        val argDocShift = argNameShift + 2

        println("Arguments:")
        for param <- info.parameters do
          val canonicalName = getNameWithMarker(param.name)
          val otherNames = param.aliasNames match {
            case Seq() => ""
            case names => names.mkString("(", ", ", ") ")
          }
          val argDoc = StringBuilder(" " * argNameShift)
          argDoc.append(s"$canonicalName $otherNames- ${param.typeName.split('.').last}")
          if param.isVarargs then argDoc.append(" (vararg)")
          else if param.hasDefault then argDoc.append(" (optional)")

          if (param.documentation.nonEmpty) {
            val shiftedDoc =
              param.documentation.split("\n").nn
                  .map(line => shiftLines(wrapLongLine(line.nn, maxUsageLineLength - argDocShift), argDocShift))
                  .mkString("\n")
            argDoc.append("\n").append(shiftedDoc)
          }

          println(argDoc)
      }
    end printExplain

    def shouldPrintDefaultHelp(names: Names, args: Seq[String]): Boolean =
      val helpIsOverridden = names.canonicalName(helpArg).isDefined
      val shortHelpIsOverridden = names.canonicalName(shortHelpArg).isDefined
      (!helpIsOverridden && args.contains(helpArg)) ||
      (!shortHelpIsOverridden && args.contains(shortHelpArg))

  end Help

  @experimental // MiMa does not check scope inherited @experimental
  private class Names(info: Info):

    checkNames()
    checkFlags()

    private lazy val namesToCanonicalName: Map[String, String] =
      info.parameters.flatMap(param =>
        val canonicalName = getNameWithMarker(param.name)
        (canonicalName -> canonicalName) +: param.aliasNames.map(_ -> canonicalName)
      ).toMap

    private lazy val canonicalFlagsNames: Set[String] =
      info.parameters.collect {
        case param if param.isFlag => getNameWithMarker(param.name)
      }.toSet

    def canonicalName(name: String): Option[String] = namesToCanonicalName.get(name)

    def isFlagName(name: String): Boolean =
      namesToCanonicalName.get(name).map(canonicalFlagsNames.contains).contains(true)

    override def toString(): String = s"Names($namesToCanonicalName)"

    private def checkNames(): Unit =
      def checkDuplicateNames() =
        val nameAndCanonicalName = info.parameters.flatMap { paramInfo =>
          (getNameWithMarker(paramInfo.name) +: paramInfo.aliasNames).map(_ -> paramInfo.name)
        }
        val nameToNames = nameAndCanonicalName.groupMap(_._1)(_._2)
        for (name, canonicalNames) <- nameToNames if canonicalNames.length > 1 do
          throw IllegalArgumentException(s"$name is used for multiple parameters: ${canonicalNames.mkString(", ")}")
      def checkValidNames() =
        def isValidArgName(name: String): Boolean =
          longArgRegex.matches(s"--$name") || shortArgRegex.matches(s"-$name")
        for param <- info.parameters do
          if !isValidArgName(param.name) then
            throw IllegalArgumentException(s"The following argument name is invalid: ${param.name}")
          for annot <- param.annotations do
            annot match
              case alias: alias if !isValidArgName(alias.name)  =>
                throw IllegalArgumentException(s"The following alias is invalid: ${alias.name}")
              case _ =>

      checkValidNames()
      checkDuplicateNames()

    private def checkFlags(): Unit =
      for param <- info.parameters if param.isFlag && param.hasDefault do
        throw IllegalArgumentException(s"@newMain flag parameters cannot have a default value. `${param.name}` has a default value.")

  end Names

end newMain

object newMain:

  /** Alias name for the parameter.
   *
   *  If the name has one character, then it is a short name (e.g. `-i`).
   *  If the name has more than one characters, then it is a long name (e.g. `--input`).
   */
  @experimental
  final class alias(val name: String) extends MainAnnotation.ParameterAnnotation

end newMain
