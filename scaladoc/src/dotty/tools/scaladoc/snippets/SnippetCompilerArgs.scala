package dotty.tools.scaladoc
package snippets

import java.nio.file.Path

case class SnippetCompilerArg(flag: SCFlags):
  def overrideFlag(f: SCFlags): SnippetCompilerArg = copy(flag = f)

enum SCFlags(val flagName: String):
  case Compile extends SCFlags("compile")
  case MacroCompile extends SCFlags("macrocompile")
  case UsingQuotes extends SCFlags("usingquotes")
  case NoCompile extends SCFlags("nocompile")
  case Fail extends SCFlags("fail")

case class SnippetCompilerArgs(scFlags: PathBased[SCFlags], defaultFlag: SCFlags):
  def get(member: Member): SnippetCompilerArg =
    member.sources
      .flatMap(s => scFlags.get(s.path).map(_.elem))
      .fold(SnippetCompilerArg(defaultFlag))(SnippetCompilerArg(_))

  def get(path: Option[Path]): SnippetCompilerArg =
    path
      .flatMap(p => scFlags.get(p).map(_.elem))
      .fold(SnippetCompilerArg(defaultFlag))(SnippetCompilerArg(_))


object SnippetCompilerArgs:
  val usage =
    """
    |Snippet compiler arguments provide a way to configure snippet type checking.
    |
    |This setting accept list of arguments in format:
    |args := arg{,arg}
    |arg := [path=]flag
    |where `path` is a prefix of the path to source files where snippets are located and `flag` is the mode in which snippets will be type checked.
    |
    |If the path is not present, the argument will be used as the default for all unmatched paths..
    |
    |Available flags:
    |compile - Enables snippet checking.
    |macrocompile - Enables snippet checking for macros. 
    |usingquotes - Enables checking snippet additionally wrapped in `scala.quoted.Quotes` impilicit scope.
    |nocompile - Disables snippet checking.
    |fail - Enables snippet checking, asserts that snippet doesn't compile.
    |
    """.stripMargin

  def load(args: List[String], defaultFlag: SCFlags = SCFlags.NoCompile)(using CompilerContext): SnippetCompilerArgs = {
    PathBased.parse[SCFlags](args)(using SCFlagsParser) match {
      case PathBased.ParsingResult(errors, res) =>
        if errors.nonEmpty then report.warning(s"""
            |Got following errors during snippet compiler args parsing:
            |$errors
            |
            |$usage
            |""".stripMargin
        )
        SnippetCompilerArgs(res, defaultFlag)
    }
  }

object SCFlagsParser extends ArgParser[SCFlags]:
  def parse(s: String): Either[String, SCFlags] = {
    SCFlags.values
      .find(_.flagName == s)
      .fold(Left(s"$s: No such flag found."))(Right(_))
  }
