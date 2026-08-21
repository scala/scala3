package dotty.tools.repl

private[repl] object ReplCommands:

  /** Column at which command descriptions start in `:help`. */
  private val DescriptionColumn = 25

  /** Minimum gap before the description, for usages overflowing [[DescriptionColumn]]. */
  private val MinDescriptionGap = 5

  private case class CommandHelp(description: String, arguments: String)

  private case class CommandDefinition(
    companion: CommandCompanion,
    help: Option[CommandHelp] = None,
    aliases: List[String] = Nil
  ):
    def name: String = companion.command
    def names: List[String] = name :: aliases

    def helpText: Option[String] = help.map: entry =>
      val usage = if entry.arguments.isEmpty then name else s"$name ${entry.arguments}"
      val padding = " " * math.max(MinDescriptionGap, DescriptionColumn - usage.length)
      val aliasText = aliases match
        case Nil => ""
        case alias :: Nil => s" (alias: $alias)"
        case many => s" (aliases: ${many.mkString(", ")})"
      s"$usage$padding${entry.description}$aliasText"

  private def command(
    companion: CommandCompanion,
    description: String,
    arguments: String = "",
    aliases: List[String] = Nil
  ): CommandDefinition =
    CommandDefinition(companion, Some(CommandHelp(description, arguments)), aliases)

  /** A command that is recognized but not listed in `:help`. */
  private def hidden(companion: CommandCompanion): CommandDefinition =
    CommandDefinition(companion)

  private val definitions = List(
    command(Help,       "print this summary"),
    command(Save,       "save replayable session to a file", "<path>"),
    command(Load,       "interpret lines in a file", "<path>"),
    command(Quit,       "exit the interpreter", aliases = List(Quit.alias)),
    command(TypeOf,     "evaluate the type of the given expression", "<expression>"),
    command(DocOf,      "print the documentation for the given expression", "<expression>"),
    command(Imports,    "show import history"),
    command(Reset,      "clear the session and start fresh with the given compiler options", "[options]"),
    command(Replay,     "reset, then re-run the session with the given compiler options", "[options]"),
    command(Settings,   "update compiler options, if possible", "<options>"),
    command(Silent,     "disable/enable automatic printing of results"),
    command(JarCmd,     "add a JAR to the classpath", "<path>"),
    command(Dep,        "resolve a dependency and make it available in the REPL", "<group>::<artifact>:<version>"),
    command(ToolkitCmd, "resolve a toolkit and make it available in the REPL", "<version>|default|<flavor>:<version>"),
    hidden(Sh),
    hidden(KindOf),
    hidden(Require),
    hidden(Paste)
  )

  private val commands = definitions.flatMap: definition =>
    definition.names.map(_ -> definition.companion)

  val names: List[String] = commands.map(_._1)

  require(names.distinct.size == names.size, "REPL command names must be unique")

  val helpText: String =
    s"""The REPL has several commands available:
       |
       |${definitions.flatMap(_.helpText).mkString("\n")}""".stripMargin

  /** Resolve a `:command` name (which may be a prefix) and its argument into a `Command`. */
  def parse(name: String, argument: String): Command =
    commands.filter((command, _) => command.startsWith(name)) match
      case Nil => UnknownCommand(name)
      case (_, companion) :: Nil => companion.parse(argument)
      case multiple => AmbiguousCommand(name, multiple.map(_._1))
