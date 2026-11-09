package dotty.tools.scaladoc
package snippets

import java.nio.file.Path

case class SnippetCompilerArg(
  flag: SCFlags,
  scalacOptions: Seq[String] = Seq.empty,
  verifyDiagnostics: Boolean = false
):
  def overrideFlag(f: SCFlags): SnippetCompilerArg = copy(flag = f)
  def withScalacOptions(opts: Seq[String]): SnippetCompilerArg = copy(scalacOptions = scalacOptions ++ opts)
  def merge(other: SnippetCompilerArg): SnippetCompilerArg =
    SnippetCompilerArg(
      other.flag,
      scalacOptions ++ other.scalacOptions,
      verifyDiagnostics || other.verifyDiagnostics
    )

enum SCFlags(val flagName: String):
  case Compile extends SCFlags("compile")
  case NoCompile extends SCFlags("nocompile")
  case Fail extends SCFlags("fail")

case class SnippetCompilerArgs(scArgs: PathBased[SnippetCompilerArg], defaultFlag: SCFlags):
  def get(member: Member): SnippetCompilerArg =
    member.sources
      .flatMap(s => scArgs.get(s.path).map(_.elem))
      .getOrElse(SnippetCompilerArg(defaultFlag))

  def get(path: Option[Path]): SnippetCompilerArg =
    path
      .flatMap(p => scArgs.get(p).map(_.elem))
      .getOrElse(SnippetCompilerArg(defaultFlag))


object SnippetCompilerArgs:
  /** Enables inline diagnostic expectation checking (`// error`, `// warn`). */
  val TestModifier = "test"

  val usage =
    s"""
    |Snippet compiler arguments provide a way to configure snippet type checking.
    |
    |This setting accepts a list of arguments in format:
    |args := arg{,arg}
    |arg := [path=]flag[+modifier]*[|scalacOption]*
    |where `path` is a prefix of the path to source files where snippets are located, `flag` is the mode in which snippets will be type checked, optional `modifier`s tweak snippet assertions, and optional `scalacOption`s (separated by `|`) are passed to the compiler.
    |
    |If the path is not present, the argument will be used as the default for all unmatched paths.
    |
    |Available flags:
    |compile - Enables snippet checking.
    |nocompile - Disables snippet checking.
    |fail - Enables snippet checking, asserts that snippet doesn't compile.
    |
    |Available modifiers:
    |$TestModifier - Enables inline diagnostic expectation checking (`// error`, `// warn`).
    """.stripMargin

  def load(args: List[String], defaultFlag: SCFlags = SCFlags.NoCompile)(using CompilerContext): SnippetCompilerArgs = {
    PathBased.parse[SnippetCompilerArg](args)(using SnippetCompilerArgParser) match {
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
  def parse(s: String): Either[String, SCFlags] =
    SCFlags.values
      .find(_.flagName == s)
      .fold(Left(s"$s: No such flag found."))(Right(_))

object SnippetCompilerArgParser extends ArgParser[SnippetCompilerArg]:
  def parse(s: String): Either[String, SnippetCompilerArg] =
    val parts = s.split("\\|")
    val flagAndModifiers = parts.head.split("\\+").toList
    val flagText = flagAndModifiers.head
    val modifiers = flagAndModifiers.tail
    val unknownModifiers = modifiers.filterNot(_ == SnippetCompilerArgs.TestModifier)
    val verifyDiagnostics = modifiers.contains(SnippetCompilerArgs.TestModifier)
    if unknownModifiers.nonEmpty then
      Left(s"${unknownModifiers.mkString(", ")}: Unknown snippet compiler modifier(s).")
    else
      SCFlagsParser.parse(flagText).map: flag =>
        SnippetCompilerArg(flag, parts.drop(1).toSeq, verifyDiagnostics)
