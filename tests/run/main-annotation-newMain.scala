// scalajs: --skip

import scala.annotation.*
import collection.mutable
import scala.util.CommandLineParser.FromString

@newMain def happyBirthday(age: Int, name: String, others: String*) =
  val suffix =
    age % 100 match
    case 11 | 12 | 13 => "th"
    case _ =>
      age % 10 match
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case _ => "th"
  val bldr = new StringBuilder(s"Happy $age$suffix birthday, $name")
  for other <- others do bldr.append(" and ").append(other)
  println(bldr)


object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("happyBirthday")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("23", "Lisa", "Peter"))
end Test



@experimental
final class newMain extends MainAnnotation[FromString, Any]:
  import newMain._
  import MainAnnotation._

  private inline val argMarker = "--"
  private inline val shortArgMarker = "-"

  /** The name of the special argument to display the method's help.
   *  If one of the method's parameters is called the same, will be ignored.
   */
  private inline val helpArg = "help"

  /** The short name of the special argument to display the method's help.
   *  If one of the method's parameters uses the same short name, will be ignored.
   */
  private inline val shortHelpArg = 'h'

  private inline val maxUsageLineLength = 120

  private var info: Info = _ // TODO remove this var


  /** A buffer for all errors */
  private val errors = new mutable.ArrayBuffer[String]

  /** Issue an error, and return an uncallable getter */
  private def error(msg: String): () => Nothing =
    errors += msg
    () => throw new AssertionError("trying to get invalid argument")

  private def getAliases(param: Parameter): Seq[String] =
    param.annotations.collect{ case a: Alias => a }.flatMap(_.aliases)

  private def getAlternativeNames(param: Parameter): Seq[String] =
    getAliases(param).filter(nameIsValid(_))

  private def getShortNames(param: Parameter): Seq[Char] =
    getAliases(param).filter(shortNameIsValid(_)).map(_(0))

  private inline def nameIsValid(name: String): Boolean =
    name.length > 1 // TODO add more checks for illegal characters

  private inline def shortNameIsValid(name: String): Boolean =
    name.length == 1 && shortNameIsValidChar(name(0))

  private inline def shortNameIsValidChar(shortName: Char): Boolean =
    ('A' <= shortName && shortName <= 'Z') || ('a' <= shortName && shortName <= 'z')

  private def getNameWithMarker(name: String | Char): String = name match {
    case c: Char => shortArgMarker + c
    case s: String if shortNameIsValid(s) => shortArgMarker + s
    case s => argMarker + s
  }

  private def getInvalidNames(param: Parameter): Seq[String | Char] =
    getAliases(param).filter(name => !nameIsValid(name) && !shortNameIsValid(name))

  def command(info: Info, args: Seq[String]): Option[Seq[String]] =
    this.info = info

    val namesToCanonicalName: Map[String, String] = info.parameters.flatMap(
      infos =>
        val names = getAlternativeNames(infos)
        val canonicalName = infos.name
        if nameIsValid(canonicalName) then (canonicalName +: names).map(_ -> canonicalName)
        else names.map(_ -> canonicalName)
    ).toMap
    val shortNamesToCanonicalName: Map[Char, String] = info.parameters.flatMap(
      infos =>
        val names = getShortNames(infos)
        val canonicalName = infos.name
        if shortNameIsValid(canonicalName) then (canonicalName(0) +: names).map(_ -> canonicalName)
        else names.map(_ -> canonicalName)
    ).toMap

    val helpIsOverridden = namesToCanonicalName.exists((name, _) => name == helpArg)
    val shortHelpIsOverridden = shortNamesToCanonicalName.exists((name, _) => name == shortHelpArg)

    val (positionalArgs, byNameArgs, invalidByNameArgs) = {
      def getCanonicalArgName(arg: String): Option[String] =
        if arg.startsWith(argMarker) && arg.length > argMarker.length then
          namesToCanonicalName.get(arg.drop(argMarker.length))
        else if arg.startsWith(shortArgMarker) && arg.length == shortArgMarker.length + 1 then
          shortNamesToCanonicalName.get(arg(shortArgMarker.length))
        else
          None

      def isArgName(arg: String): Boolean =
        val isFullName = arg.startsWith(argMarker)
        val isShortName = arg.startsWith(shortArgMarker) && arg.length == shortArgMarker.length + 1 && shortNameIsValidChar(arg(shortArgMarker.length))
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

    val argStrings: Seq[Seq[String]] =
      for paramInfo <- info.parameters yield {
        if (paramInfo.isVarargs) {
          val byNameGetters = byNameArgs.getOrElse(paramInfo.name, Seq())
          val positionalGetters = positionalArgs.removeAll()
          // First take arguments passed by name, then those passed by position
          byNameGetters ++ positionalGetters
        } else {
          byNameArgs.get(paramInfo.name) match
          case Some(Nil) =>
            throw AssertionError(s"${paramInfo.name} present in byNameArgs, but it has no argument value")
          case Some(argValues) =>
            if argValues.length > 1 then
              // Do not accept multiple values
              // Remove this test to take last given argument
              error(s"more than one value for ${paramInfo.name}: ${argValues.mkString(", ")}")
              Nil
            else
              List(argValues.last)
          case None =>
            if positionalArgs.length > 0 then
              List(positionalArgs.dequeue())
            else if paramInfo.hasDefault then
              Nil
            else
              error(s"missing argument for ${paramInfo.name}")
              Nil
        }
      }

    // Check aliases unicity
    val nameAndCanonicalName = info.parameters.flatMap {
      case paramInfo => (paramInfo.name +: getAlternativeNames(paramInfo) ++: getShortNames(paramInfo)).map(_ -> paramInfo.name)
    }
    val nameToCanonicalNames = nameAndCanonicalName.groupMap(_._1)(_._2)

    for (name, canonicalNames) <- nameToCanonicalNames if canonicalNames.length > 1 do
      throw IllegalArgumentException(s"$name is used for multiple parameters: ${canonicalNames.mkString(", ")}")

    // Check aliases validity
    val problematicNames = info.parameters.flatMap(getInvalidNames)
    if problematicNames.length > 0 then
      throw IllegalArgumentException(s"The following aliases are invalid: ${problematicNames.mkString(", ")}")

    // Handle unused and invalid args
    for (remainingArg <- positionalArgs) error(s"unused argument: $remainingArg")
    for (invalidArg <- invalidByNameArgs) error(s"unknown argument name: $invalidArg")

    val displayHelp =
      (!helpIsOverridden && args.contains(getNameWithMarker(helpArg))) ||
      (!shortHelpIsOverridden && args.contains(getNameWithMarker(shortHelpArg)))

    if displayHelp then
      usage()
      println()
      explain()
      None
    else if errors.nonEmpty then
      for msg <- errors do println(s"Error: $msg")
      usage()
      None
    else
      Some(argStrings.flatten)
  end command

  private def usage(): Unit =
    def argsUsage: Seq[String] =
      for (infos <- info.parameters)
      yield {
        val canonicalName = getNameWithMarker(infos.name)
        val shortNames = getShortNames(infos).map(getNameWithMarker)
        val alternativeNames = getAlternativeNames(infos).map(getNameWithMarker)
        val namesPrint = (canonicalName +: alternativeNames ++: shortNames).mkString("[", " | ", "]")
        val shortTypeName = infos.typeName.split('.').last
        if infos.isVarargs then s"[<$shortTypeName> [<$shortTypeName> [...]]]"
        else if infos.hasDefault then s"[$namesPrint <$shortTypeName>]"
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

    val usageBeginning = s"Usage: ${info.name} "
    val argsOffset = usageBeginning.length
    val usages = wrapArgumentUsages(argsUsage, maxUsageLineLength - argsOffset)

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

    if (info.documentation.nonEmpty)
      println(wrapLongLine(info.documentation, maxUsageLineLength).mkString("\n"))
    if (info.parameters.nonEmpty) {
      val argNameShift = 2
      val argDocShift = argNameShift + 2

      println("Arguments:")
      for infos <- info.parameters do
        val canonicalName = getNameWithMarker(infos.name)
        val shortNames = getShortNames(infos).map(getNameWithMarker)
        val alternativeNames = getAlternativeNames(infos).map(getNameWithMarker)
        val otherNames = (alternativeNames ++: shortNames) match {
          case Seq() => ""
          case names => names.mkString("(", ", ", ") ")
        }
        val argDoc = StringBuilder(" " * argNameShift)
        argDoc.append(s"$canonicalName $otherNames- ${infos.typeName.split('.').last}")
        if infos.isVarargs then argDoc.append(" (vararg)")
        else if infos.hasDefault then argDoc.append(" (optional)")

        if (infos.documentation.nonEmpty) {
          val shiftedDoc =
            infos.documentation.split("\n").nn
                .map(line => shiftLines(wrapLongLine(line.nn, maxUsageLineLength - argDocShift), argDocShift))
                .mkString("\n")
          argDoc.append("\n").append(shiftedDoc)
        }

        println(argDoc)
    }
  end explain

  private def convert[T](argName: String, arg: String, p: FromString[T]): () => T =
    p.fromStringOption(arg) match
      case Some(t) => () => t
      case None => error(s"invalid argument for $argName: $arg")

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using p: FromString[T]): () => T = {
    if arg.nonEmpty then convert(param.name, arg, p)
    else defaultArgument match
      case Some(defaultGetter) => defaultGetter
      case None => error(s"missing argument for ${param.name}")
  }

  def varargGetter[T](param: Parameter, args: Seq[String])(using p: FromString[T]): () => Seq[T] = {
    val getters = args.map(arg => convert(param.name, arg, p))
    () => getters.map(_())
  }

  def run(execProgram: () => Any): Unit = {
    if errors.nonEmpty then
      for msg <- errors do println(s"Error: $msg")
      usage()
    else
      execProgram()
  }

end newMain

object newMain:
  @experimental
  final class Alias(val aliases: String*) extends MainAnnotation.ParameterAnnotation
end newMain
