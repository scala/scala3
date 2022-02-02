package dotty.tools.dotc
package config

import Settings._
import core.Contexts._
import printing.Highlighting

import scala.util.chaining.given
import scala.PartialFunction.cond

trait CliCommand:

  type ConcreteSettings <: CommonScalaSettings with Settings.SettingGroup

  def versionMsg: String

  def ifErrorsMsg: String

  /** The name of the command */
  def cmdName: String

  def isHelpFlag(using settings: ConcreteSettings)(using SettingsState): Boolean

  def helpMsg(using settings: ConcreteSettings)(using SettingsState, Context): String

  private def explainAdvanced = """
    |-- Notes on option parsing --
    |Boolean settings are always false unless set.
    |Where multiple values are accepted, they should be comma-separated.
    |  example: -Xplugin:plugin1,plugin2
    |<phases> means one or a comma-separated list of:
    |  - (partial) phase names with an optional "+" suffix to include the next phase
    |  - the string "all"
    |  example: -Xprint:all prints all phases.
    |  example: -Xprint:typer,mixin prints the typer and mixin phases.
    |  example: -Ylog:erasure+ logs the erasure phase and the phase after the erasure phase.
    |           This is useful because during the tree transform of phase X, we often
    |           already are in phase X + 1.
  """

  /** Distill arguments into summary detailing settings, errors and files to main */
  def distill(args: Array[String], sg: Settings.SettingGroup)(ss: SettingsState = sg.defaultState)(using Context): ArgsSummary =

    // expand out @filename to the contents of that filename
    def expandedArguments = args.toList flatMap {
      case x if x startsWith "@"  => CommandLineParser.expandArg(x)
      case x                      => List(x)
    }

    sg.processArguments(expandedArguments, processAll = true, settingsState = ss)
  end distill

  /** Creates a help message for a subset of options based on cond */
  protected def availableOptionsMsg(cond: Setting[?] => Boolean)(using settings: ConcreteSettings)(using SettingsState): String =
    val ss = (settings.allSettings filter cond).toList sortBy (_.name)
    val maxNameWidth = 30
    val nameWidths = ss.map(_.name.length).filter(_ < maxNameWidth)
    val width = if nameWidths.nonEmpty then nameWidths.max else maxNameWidth
    val terminalWidth = settings.pageWidth.value
    val (nameWidth, descriptionWidth) = {
      val w1 =
        if width < maxNameWidth then width
        else maxNameWidth
      val w2 =
        if terminalWidth < w1 + maxNameWidth then 0
        else terminalWidth - w1 - 1
      (w1, w2)
    }
    def formatName(name: String) =
      if name.length <= nameWidth then ("%-" + nameWidth + "s") format name
      else (name + "\n%-" + nameWidth + "s") format ""
    def formatDescription(text: String): String =
      if descriptionWidth == 0 then text
      else if text.length < descriptionWidth then text
      else {
        val inx = text.substring(0, descriptionWidth).lastIndexOf(" ")
        if inx < 0 then text
        else
          val str = text.substring(0, inx)
          s"${str}\n${formatName("")} ${formatDescription(text.substring(inx + 1))}"
      }
    def formatSetting(name: String, value: String) =
      if (value.nonEmpty)
        // the format here is helping to make empty padding and put the additional information exactly under the description.
        s"\n${formatName("")} $name: $value."
      else
        ""
    def helpStr(s: Setting[?]) =
      def defaultValue = s.default match
        case _: Int | _: String => s.default.toString
        case _ =>
          // For now, skip the default values that do not make sense for the end user.
          // For example 'false' for the version command.
          ""
      s"${formatName(s.name)} ${formatDescription(shortHelp(s))}${formatSetting("Default", defaultValue)}${formatSetting("Choices", s.legalChoices)}"
    ss.map(helpStr).mkString("", "\n", s"\n${formatName("@<file>")} ${formatDescription("A text file containing compiler arguments (options and source files).")}\n")
  end availableOptionsMsg

  protected def shortUsage: String = s"Usage: $cmdName <options> <source files>"

  protected def createUsageMsg(label: String, shouldExplain: Boolean, cond: Setting[?] => Boolean)(using settings: ConcreteSettings)(using SettingsState): String =
    val prefix = List(
      Some(shortUsage),
      Some(explainAdvanced) filter (_ => shouldExplain),
      Some(label + " options include:")
    ).flatten mkString "\n"

    prefix + "\n" + availableOptionsMsg(cond)

  protected def isStandard(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    !isVerbose(s) && !isWarning(s) && !isAdvanced(s) && !isPrivate(s) || s.name == "-Werror" || s.name == "-Wconf"
  protected def isVerbose(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-V") && s.name != "-V"
  protected def isWarning(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-W") && s.name != "-W" || s.name == "-Xlint"
  protected def isAdvanced(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-X") && s.name != "-X"
  protected def isPrivate(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    s.name.startsWith("-Y") && s.name != "-Y"
  protected def shortHelp(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): String =
    s.description.linesIterator.next()
  protected def isHelping(s: Setting[?])(using settings: ConcreteSettings)(using SettingsState): Boolean =
    cond(s.value) {
      case ss: List[?] if s.isMultivalue => ss.contains("help")
      case s: String                     => "help" == s
    }

  /** Messages explaining usage and options */
  protected def usageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("where possible standard", shouldExplain = false, isStandard)
  protected def vusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible verbose", shouldExplain = true, isVerbose)
  protected def wusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible warning", shouldExplain = true, isWarning)
  protected def xusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible advanced", shouldExplain = true, isAdvanced)
  protected def yusageMessage(using settings: ConcreteSettings)(using SettingsState) =
    createUsageMsg("Possible private", shouldExplain = true, isPrivate)

  /** Used for the formatted output of -Xshow-phases */
  protected def phasesMessage(using Context): String =
    val phases = new Compiler().phases
    val formatter = Columnator("phase name", "description", maxField = 25, separation = 2)
    formatter(phases.map(mega => mega.map(p => (p.phaseName, p.description))))

  /** Provide usage feedback on argument summary, assuming that all settings
   *  are already applied in context.
   *  @return  Either Some list of files passed as arguments or None if further processing should be interrupted.
   */
  def checkUsage(summary: ArgsSummary, sourcesRequired: Boolean)(using settings: ConcreteSettings)(using SettingsState, Context): Option[List[String]] =
    // Print all warnings encountered during arguments parsing
    summary.warnings.foreach(report.warning(_))

    if summary.errors.nonEmpty then
      summary.errors foreach (report.error(_))
      report.echo(ifErrorsMsg)
      None
    else if settings.version.value then
      report.echo(versionMsg)
      None
    else if isHelpFlag then
      report.echo(helpMsg)
      None
    else if (sourcesRequired && summary.arguments.isEmpty)
      report.echo(usageMessage)
      None
    else
      Some(summary.arguments)

  extension [T](setting: Setting[T])
    protected def value(using ss: SettingsState): T = setting.valueIn(ss)

  extension (s: String)
    def padLeft(width: Int): String = StringBuilder().tap(_.append(" " * (width - s.length)).append(s)).toString

  // Formatting for -help and -Vphases in two columns, handling long field1 and wrapping long field2
  class Columnator(heading1: String, heading2: String, maxField: Int, separation: Int = 1):
    def apply(texts: List[List[(String, String)]])(using Context): String = StringBuilder().tap(columnate(_, texts)).toString

    private def columnate(sb: StringBuilder, texts: List[List[(String, String)]])(using Context): Unit =
      import scala.util.Properties.{lineSeparator => EOL}
      import Highlighting.*
      val colors = Seq(Green(_), Yellow(_), Magenta(_), Cyan(_), Red(_))
      val nocolor = texts.length == 1
      def color(index: Int): String => Highlight = if nocolor then NoColor(_) else colors(index % colors.length)
      val maxCol = ctx.settings.pageWidth.value
      val field1 = maxField.min(texts.flatten.map(_._1.length).filter(_ < maxField).max) // widest field under maxField
      val field2 = if field1 + separation + maxField < maxCol then maxCol - field1 - separation else 0 // skinny window -> terminal wrap
      val separator = " " * separation
      def formatField1(text: String): String = if text.length <= field1 then text.padLeft(field1) else EOL + "".padLeft(field1)
      def formatField2(text: String): String =
        def loopOverField2(fld: String): String =
          if field2 == 0 || fld.length <= field2 then fld
          else
            fld.lastIndexOf(" ", field2) match
              case -1 => fld
              case i  =>
                val (prefix, rest) = fld.splitAt(i)
                s"${prefix}${EOL}${formatField1("")}${separator}${loopOverField2(rest.trim)}"
        def loopOverFields2(rest: List[String]): String =
          rest match
            case h :: t => loopOverField2(h.trim) + loopOverFields2(t)
            case Nil => ""
        loopOverFields2(text.split("\n").toList)
      end formatField2
      def format(first: String, second: String, index: Int, colorPicker: Int => String => Highlight) =
        sb.append(colorPicker(index)(formatField1(first)).show)
          .append(separator)
          .append(formatField2(second))
          .append(EOL): Unit
      def fancy(first: String, second: String, index: Int) = format(first, second, index, color)
      def plain(first: String, second: String) = format(first, second, 0, _ => NoColor(_))

      if heading1.nonEmpty then
        plain(heading1, heading2)
        plain("-" * heading1.length, "-" * heading2.length)

      def emit(index: Int)(textPair: (String, String)): Unit = fancy(textPair._1, textPair._2, index)
      def group(index: Int)(body: Int => Unit): Unit =
        if !ctx.useColors then plain(s"{", "")
        body(index)
        if !ctx.useColors then plain(s"}", "")

      texts.zipWithIndex.foreach { (text, index) =>
        text match
          case List(single) => emit(index)(single)
          case Nil          =>
          case mega         => group(index)(i => mega.foreach(emit(i)))
      }
  end Columnator
