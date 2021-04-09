package dotty.tools.scaladoc
package snippets

import java.nio.file.Path

case class SnippetCompilerArg(flag: SCFlags, debug: Boolean):
  def overrideFlag(f: SCFlags): SnippetCompilerArg = copy(flag = f)

sealed trait SCFlags(val flagName: String)

object SCFlags:
  case object Compile extends SCFlags("compile")
  case object NoCompile extends SCFlags("nocompile")
  case object Fail extends SCFlags("fail")

  def values: Seq[SCFlags] = Seq(Compile, NoCompile, Fail)

case class SnippetCompilerArgs(scFlags: PathBased[SCFlags], val debug: Boolean, defaultFlag: SCFlags):
  def get(member: Member): SnippetCompilerArg =
    member.sources
      .flatMap(s => scFlags.get(s.path).map(_.elem))
      .fold(SnippetCompilerArg(defaultFlag, debug))(SnippetCompilerArg(_, debug))

  def get(path: Option[Path]): SnippetCompilerArg =
    path
      .flatMap(p => scFlags.get(p).map(_.elem))
      .fold(SnippetCompilerArg(defaultFlag, debug))(SnippetCompilerArg(_, debug))


object SnippetCompilerArgs:
  val usage =
    """
    |Snippet compiler arguments provide a way to configure snippet checking.
    |
    |This setting accept list of arguments in format:
    |args := arg{,arg}
    |arg := [path=]flag
    |where path is a prefix of source paths to members to which argument should be set.
    |
    |If path is not present, argument will be used as default.
    |
    |Available flags:
    |compile - Enables snippet checking.
    |nocompile - Disables snippet checking.
    |fail - Enables snippet checking, asserts that snippet doesn't compile.
    |
    """.stripMargin

  val debugUsage = """
  |Setting this option causes snippet compiler to print snippet as it is compiled (after wrapping).
  """.stripMargin

  def load(args: List[String], debug: Boolean, defaultFlag: SCFlags = SCFlags.NoCompile)(using CompilerContext): SnippetCompilerArgs = {
    PathBased.parse[SCFlags](args)(using SCFlagsParser) match {
      case PathBased.ParsingResult(errors, res) =>
        if errors.nonEmpty then report.warning(s"""
            |Got following errors during snippet compiler args parsing:
            |$errors
            |
            |$usage
            |""".stripMargin
        )
        SnippetCompilerArgs(res, debug, defaultFlag)
    }
  }

object SCFlagsParser extends ArgParser[SCFlags]:
  def parse(s: String): Either[String, SCFlags] = {
    SCFlags.values
      .find(_.flagName == s)
      .fold(Left(s"$s: No such flag found."))(Right(_))
  }
