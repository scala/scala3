package dotty.tools.scaladoc
package snippets

case class SnippetCompilerArg(flags: Set[SCFlags]):
  def is(flag: SCFlags): Boolean = flags.contains(flag)

object SnippetCompilerArg:
  def default: SnippetCompilerArg = SnippetCompilerArg(
    Set(SCFlags.NoCompile)
  )

enum SCFlags(val flagName: String, val forbiddenFlags: Set[SCFlags]):
  case Compile extends SCFlags("compile", Set())
  case NoCompile extends SCFlags("nocompile", Set())

case class SnippetCompilerArgs(scArgs: PathBased[SnippetCompilerArg]):
  def get(member: Member): Option[SnippetCompilerArg] = member.sources.flatMap(s => scArgs.get(s.path).map(_.elem))

object SnippetCompilerArgs:
  val usage =
    """
    |Snippet compiler arguments provide a way to configure snippet checking.
    |
    |This setting accept list of arguments in format:
    |arg := [path=]flag{&flag}
    |where path is a prefix of source paths to members to which argument should be set.
    |
    |If path is not present, argument will be used as default.
    |
    |Available flags:
    |compile - Enables snippet checking. Cannot be used with nocompile.
    |nocompile - Disables snippet checking. Cannot be used with compile.
    |
    """.stripMargin

  def load(args: List[String])(using CompilerContext): SnippetCompilerArgs = {
    PathBased.parse[SnippetCompilerArg](args)(using SnippetCompilerArgParser) match {
      case PathBased.ParsingResult(errors, res) =>
        if errors.nonEmpty then report.warning(
          s"""Got following errors during snippet compiler args parsing:
            |$errors
            |
            |${usage}
            |""".stripMargin
        )
        SnippetCompilerArgs(res)
    }
  }

object SnippetCompilerArgParser extends ArgParser[SnippetCompilerArg]:
  def parse(s: String): Either[String, SnippetCompilerArg] = {
    val flagStrings = s.split("&")
    val (parsed, errors) = flagStrings.map(flag => SCFlags.values.find(_.flagName == flag).fold(
        Left(s"$flag: No such flag found.")
      )(
        Right(_)
      )
    ).partition(_.isRight)

    val (flags, errors2) = parsed.collect {
      case Right(flag) => flag
    } match {
      case list => list.map(f =>
        list.find(elem => f.forbiddenFlags.contains(elem)) match {
          case Some(forbiddenElem) => Left(s"${f.flagName}: Cannot be used with flag: ${forbiddenElem.flagName}")
          case None => Right(f)
        }
      ).partition(_.isRight)
    }

    val checkedFlags = flags.collect {
      case Right(flag) => flag
    }.toSet

    val allErrors = (errors ++ errors2).collect {
      case Left(error) => error
    }.toList

    if !allErrors.isEmpty then Left(allErrors.mkString("\n")) else Right(SnippetCompilerArg(checkedFlags))
  }
