package dotty.tools.scaladoc
package tasty.comments

import java.nio.file.Path

enum CommentSyntax:
  case Wiki
  case Markdown

object CommentSyntax:
  object CommentSyntaxParser extends ArgParser[CommentSyntax]:
    def parse(s: String): Either[String, CommentSyntax] = s match
        case "wiki" => Right(CommentSyntax.Wiki)
        case "markdown" => Right(CommentSyntax.Markdown)
        case _ => Left(s"No such syntax found.")

  val default = CommentSyntax.Markdown

case class CommentSyntaxArgs(csFormats: PathBased[CommentSyntax]):
  def get(path: Option[Path]): CommentSyntax =
    path
      .flatMap(p => csFormats.get(p).map(_.elem))
      .getOrElse(CommentSyntax.default)

object CommentSyntaxArgs:
  val usage =
    """
    |Comment Syntax arguments provide a way to set comment syntax for specified paths.
    |
    |This setting accepts list of arguments in format:
    |args := arg{,arg}
    |arg := [path=]syntax
    |where `path` is a prefix of the path to source files that will have a specific comment syntax set and `syntax` specifies the one used.
    |
    |If the path is not present, the argument will be used as the default for all unmatched paths.
    |
    |Available syntaxes:
    |markdown
    |wiki
    |
    """.stripMargin

  def load(args: List[String])(using CompilerContext): CommentSyntaxArgs = {
    PathBased.parse[CommentSyntax](args)(using CommentSyntax.CommentSyntaxParser) match {
      case PathBased.ParsingResult(errors, res) =>
        if errors.nonEmpty then report.warning(s"""
            |Got following errors during comment syntax args parsing:
            |$errors
            |
            |$usage
            |""".stripMargin
        )
        CommentSyntaxArgs(res)
    }
  }