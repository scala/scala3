package dotty.tools.scaladoc

import java.nio.file.Path
import java.nio.file.Paths
import dotty.tools.dotc.core.Contexts.Context
import scala.util.matching.Regex

def pathToString(p: Path) =
  import scala.jdk.CollectionConverters._
  // !!! gives wrong result for absolute paths!
  p.iterator.asScala.mkString("/")

trait SourceLink:
  val path: Option[Path] = None
  def render(memberName: String, path: Path, operation: String, line: Option[Int], optionalRevision: Option[String]): String

case class TemplateSourceLink(val urlTemplate: String) extends SourceLink:
  override val path: Option[Path] = None
  override def render(memberName: String, path: Path, operation: String, line: Option[Int], optionalRevision: Option[String]): String =
    val pathString = "/" + pathToString(path)
    val mapping = Map(
      "\\{\\{ path \\}\\}".r -> pathString,
      "\\{\\{ line \\}\\}".r -> line.fold("")(_.toString),
      "\\{\\{ ext \\}\\}".r -> Some(
        pathString).filter(_.lastIndexOf(".") != -1).fold("")(p => p.substring(p.lastIndexOf("."))
      ),
      "\\{\\{ path_no_ext \\}\\}".r -> Some(
        pathString).filter(_.lastIndexOf(".") != -1).fold(pathString)(p => p.substring(0, p.lastIndexOf("."))
      ),
      "\\{\\{ name \\}\\}".r -> memberName
    )
    mapping.foldLeft(urlTemplate) {
      case (sourceLink, (regex, value)) => regex.replaceAllIn(sourceLink, Regex.quoteReplacement(value))
    }


case class WebBasedSourceLink(prefix: String, revision: String, subPath: String) extends SourceLink:
  override val path: Option[Path] = None
  override def render(memberName: String, path: Path, operation: String, line: Option[Int], optionalRevision: Option[String] = None): String =
    val action = if operation == "view" then "blob" else operation
    val finalRevision = optionalRevision.getOrElse(revision)
    val linePart = line.fold("")(l => s"#L$l")
    s"$prefix/$action/$finalRevision$subPath/${pathToString(path)}$linePart"

class SourceLinkParser(revision: Option[String]) extends ArgParser[SourceLink]:
  val KnownProvider = raw"(\w+):\/\/([^\/#]+)\/([^\/#]+)(\/[^\/#]+)?(#.+)?".r
  val BrokenKnownProvider = raw"(\w+):\/\/.+".r
  val ScalaDocPatten = raw"€\{(TPL_NAME|TPL_OWNER|FILE_PATH|FILE_EXT|FILE_LINE|FILE_PATH_EXT)\}".r
  val SupportedScalaDocPatternReplacements = Map(
    "€{FILE_PATH_EXT}" -> "{{ path }}",
    "€{FILE_LINE}" -> "{{ line }}",
    "€{TPL_NAME}" -> "{{ name }}",
    "€{FILE_EXT}" -> "{{ ext }}",
    "€{FILE_PATH}" -> "{{ path_no_ext }}"
  )

  def githubPrefix(org: String, repo: String) = s"https://github.com/$org/$repo"

  def gitlabPrefix(org: String, repo: String) = s"https://gitlab.com/$org/$repo/-"


  private def parseLinkDefinition(s: String): Option[SourceLink] = ???

  def parse(string: String): Either[String, SourceLink] =
    val res = string match
      case KnownProvider(name, organization, repo, rawRevision, rawSubPath) =>
        val subPath = Option(rawSubPath).fold("")("/" + _.drop(1))
        val pathRev = Option(rawRevision).map(_.drop(1)).orElse(revision)

        def withRevision(template: String => SourceLink) =
          pathRev.fold(Left(s"No revision provided"))(r => Right(template(r)))

        name match
          case "github" =>
            withRevision(rev =>
              WebBasedSourceLink(githubPrefix(organization, repo), rev, subPath))
          case "gitlab" =>
            withRevision(rev =>
              WebBasedSourceLink(gitlabPrefix(organization, repo), rev, subPath))
          case other =>
            Left(s"'$other' is not a known provider, please provide full source path template.")
      case BrokenKnownProvider("gitlab" | "github") =>
        Left(s"Does not match known provider syntax: `<name>://organization/repository`")
      case scaladocSetting if ScalaDocPatten.findFirstIn(scaladocSetting).nonEmpty =>
        val all = ScalaDocPatten.findAllIn(scaladocSetting)
        val (supported, unsupported) = all.partition(SupportedScalaDocPatternReplacements.contains)
        if unsupported.nonEmpty then Left(s"Unsupported patterns from scaladoc format are used: ${unsupported.mkString(" ")}")
        else Right(TemplateSourceLink(supported.foldLeft(string)((template, pattern) =>
          template.replace(pattern, SupportedScalaDocPatternReplacements(pattern)))))
      case other =>
        Left("Does not match any implemented source link syntax")
    res match {
      case Left(error) => Left(s"'$string': $error")
      case other => other
    }


type Operation = "view" | "edit"

class SourceLinks(val sourceLinks: PathBased[SourceLink]):
  def pathTo(rawPath: Path, memberName: String = "", line: Option[Int] = None, operation: Operation = "view", optionalRevision: Option[String] = None): Option[String] =
    sourceLinks.get(rawPath).map(res => res.elem.render(memberName, res.path, operation, line, optionalRevision))

  def pathTo(member: Member): Option[String] =
    member.sources.flatMap(s => pathTo(s.path, member.name, Option(s.lineNumber).map(_ + 1)))

object SourceLinks:
  val usage =
    """Source links provide a mapping between file in documentation and code repository.
      |
      |Accepted formats:
      |<sub-path>=<source-link>
      |<source-link>
      |
      |where <source-link> is one of following:
      | - `github://<organization>/<repository>[/revision][#subpath]`
      |     will match https://github.com/$organization/$repository/[blob|edit]/$revision[/$subpath]/$filePath[$lineNumber]
      |     when revision is not provided then requires revision to be specified as argument for scaladoc
      | - `gitlab://<organization>/<repository>`
      |     will match https://gitlab.com/$organization/$repository/-/[blob|edit]/$revision[/$subpath]/$filePath[$lineNumber]
      |     when revision is not provided then requires revision to be specified as argument for scaladoc
      | - <scaladoc-template>
      |
      |<scaladoc-template> is a format for `doc-source-url` parameter scaladoc.
      |NOTE: We only supports `€{FILE_PATH_EXT}`, `€{TPL_NAME}`, `€{FILE_EXT}`,
      | €{FILE_PATH}, and €{FILE_LINE} patterns
      |
      |
      |Template can be defined only by subset of sources defined by path prefix represented by `<sub-path>`.
      |In such case paths used in templates will be relativized against `<sub-path>`""".stripMargin

  def load(config: Seq[String], revision: Option[String], projectRoot: Path = Paths.get("").toAbsolutePath)(using CompilerContext): SourceLinks =
    PathBased.parse(config, projectRoot)(using SourceLinkParser(revision)) match {
      case PathBased.ParsingResult(errors, sourceLinks) =>
        if errors.nonEmpty then report.warning(
          s"""Following templates has invalid format:
            |$errors
            |
            |${SourceLinks.usage}
            |""".stripMargin
        )
        SourceLinks(sourceLinks)
    }
