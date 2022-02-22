package dotty.tools.dotc
package config

import Settings._
import core.Contexts._

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
    val nameWidths = ss.map(_.name.length).partition(_ < maxNameWidth)._1
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
  protected def phasesMessage(using ctx: Context): String =
  
    val phases = new Compiler().phases
    val nameLimit = 25 
    val maxCol = ctx.settings.pageWidth.value
    val maxName = phases.flatten.map(_.phaseName.length).max
    val width = maxName.min(nameLimit)
    val maxDesc = maxCol - (width + 6)
    val fmt = s"%${width}.${width}s  %.${maxDesc}s%n"

    val sb = new StringBuilder
    sb ++= fmt.format("phase name", "description")
    sb ++= fmt.format("----------", "-----------")

    phases.foreach {
      case List(single) =>
        sb ++= fmt.format(single.phaseName, single.description)
      case Nil => ()
      case more =>
        sb ++= fmt.format(s"{", "")
        more.foreach { mini => sb ++= fmt.format(mini.phaseName, mini.description) }
        sb ++= fmt.format(s"}", "")
    }
    sb.mkString


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
