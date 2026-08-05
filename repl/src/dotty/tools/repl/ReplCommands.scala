package dotty.tools.repl

private[repl] object ReplCommands:

  /** Column at which command descriptions start in `:help`. */
  private val DescriptionColumn = 25

  /** Minimum gap before the description, for usages overflowing [[DescriptionColumn]]. */
  private val MinDescriptionGap = 5

  private case class CommandHelp(description: String, arguments: String)

  private case class CommandDefinition(
    name: String,
    parse: String => Command,
    help: Option[CommandHelp] = None,
    aliases: List[String] = Nil
  ):
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
    name: String,
    parse: String => Command,
    description: String,
    arguments: String = "",
    aliases: List[String] = Nil
  ): CommandDefinition =
    CommandDefinition(name, parse, Some(CommandHelp(description, arguments)), aliases)

  /** A command that is recognized but not listed in `:help`. */
  private def hidden(name: String, parse: String => Command): CommandDefinition =
    CommandDefinition(name, parse)

  private val definitions = List(
    command(Help.command,     _ => Help,      "print this summary"),
    command(Save.command,     Save.apply,     "save replayable session to a file", "<path>"),
    command(Load.command,     Load.apply,     "interpret lines in a file", "<path>"),
    command(Quit.command,     _ => Quit,      "exit the interpreter", aliases = List(Quit.alias)),
    command(TypeOf.command,   TypeOf.apply,   "evaluate the type of the given expression", "<expression>"),
    command(DocOf.command,    DocOf.apply,    "print the documentation for the given expression", "<expression>"),
    command(Imports.command,  _ => Imports,   "show import history"),
    command(Reset.command,    Reset.apply,    "clear the session and start fresh with the given compiler options", "[options]"),
    command(Replay.command,   Replay.apply,   "reset, then re-run the session with the given compiler options", "[options]"),
    command(Settings.command, Settings.apply, "update compiler options, if possible", "<options>"),
    command(Silent.command,   _ => Silent,    "disable/enable automatic printing of results"),
    command(JarCmd.command,   JarCmd.apply,   "add a JAR to the classpath", "<path>"),
    command(Dep.command,      Dep.apply,      "resolve a dependency and make it available in the REPL", "<group>::<artifact>:<version>"),
    hidden(Sh.command,        Sh.apply),
    hidden(KindOf.command,    KindOf.apply),
    hidden(Require.command,   Require.apply),
    hidden(Paste.command,     _ => Paste)
  )

  private val commands = definitions.flatMap: definition =>
    definition.names.map(_ -> definition.parse)

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
      case (_, handler) :: Nil => handler(argument)
      case multiple => AmbiguousCommand(name, multiple.map(_._1))
