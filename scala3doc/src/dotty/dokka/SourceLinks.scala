package dotty.dokka

import java.nio.file.Path
import java.nio.file.Paths
import liqp.Template
import dotty.dokka.model.api._

case class SourceLink(val path: Option[Path], val urlTemplate: Template)

object SourceLink:
  val SubPath = "([^=]+)=(.+)".r
  val KnownProvider = raw"(\w+):\/\/([^\/]+)\/([^\/]+)".r
  val BrokenKnownProvider = raw"(\w+):\/\/.+".r
  val ScalaDocPatten = raw"€\{(TPL_NAME|TPL_NAME|FILE_PATH|FILE_EXT|FILE_LINE|FILE_PATH_EXT)\}".r
  val SupportedScalaDocPatternReplacements = Map(
    "€{FILE_PATH_EXT}" -> "{{ path }}",
    "€{FILE_LINE}" -> "{{ line }}"
  )

  def githubTemplate(organization: String, repo: String)(revision: String) =
    s"""https://github.com/$organization/$repo/{{ operation | replace: "view", "blob" }}/$revision/{{ path }}#L{{ line }}""".stripMargin

  def gitlabTemplate(organization: String, repo: String)(revision: String) =
    s"""https://gitlab.com/$organization/$repo/-/{{ operation | replace: "view", "blob" }}/$revision/{{ path }}#L{{ line }}"""


  private def parseLinkDefinition(s: String): Option[SourceLink] = ???

  def parse(string: String, revision: Option[String]): Either[String, SourceLink] =
    def asTemplate(template: String) =
       try Right(SourceLink(None,Template.parse(template))) catch
            case e: RuntimeException =>
              Left(s"Failed to parse template: ${e.getMessage}")

    string match
      case KnownProvider(name, organization, repo) =>
        def withRevision(template: String => String) =
            revision.fold(Left(s"No revision provided"))(rev => Right(SourceLink(None, Template.parse(template(rev)))))

        name match
          case "github" =>
            withRevision(githubTemplate(organization, repo))
          case "gitlab" =>
            withRevision(gitlabTemplate(organization, repo))
          case other =>
            Left(s"'$other' is not a known provider, please provide full source path template.")

      case SubPath(prefix, config) =>
        parse(config, revision) match
          case l: Left[String, _] => l
          case Right(SourceLink(Some(prefix), _)) =>
            Left(s"Source path $string has duplicated subpath setting (scm template can not contains '=')")
          case Right(SourceLink(None, template)) =>
            Right(SourceLink(Some(Paths.get(prefix)), template))
      case BrokenKnownProvider("gitlab" | "github") =>
        Left(s"Does not match known provider syntax: `<name>://organization/repository`")
      case scaladocSetting if ScalaDocPatten.findFirstIn(scaladocSetting).nonEmpty =>
        val all = ScalaDocPatten.findAllIn(scaladocSetting)
        val (supported, unsupported) = all.partition(SupportedScalaDocPatternReplacements.contains)
        if unsupported.nonEmpty then Left(s"Unsupported patterns from scaladoc format are used: ${unsupported.mkString(" ")}")
        else asTemplate(supported.foldLeft(string)((template, pattern) =>
          template.replace(pattern, SupportedScalaDocPatternReplacements(pattern))))

      case template => asTemplate(template)


type Operation = "view" | "edit"

case class SourceLinks(links: Seq[SourceLink], projectRoot: Path):
  def pathTo(rawPath: Path, line: Option[Int] = None, operation: Operation = "view"): Option[String] =
    def resolveRelativePath(path: Path) =
      links.find(_.path.forall(p => path.startsWith(p))).map { link =>
        val config = java.util.HashMap[String, Object]()
        val pathString = path.toString.replace('\\', '/')
        config.put("path", pathString)
        line.foreach(l => config.put("line", l.toString))
        config.put("operation", operation)

        link.urlTemplate.render(config)
      }

    if rawPath.isAbsolute then
      if rawPath.startsWith(projectRoot) then resolveRelativePath(projectRoot.relativize(rawPath))
      else None
    else resolveRelativePath(rawPath)

  def pathTo(member: Member): Option[String] =
    member.sources.flatMap(s => pathTo(Paths.get(s.path), Option(s.lineNumber).map(_ + 1)))

object SourceLinks:

  val usage =
    """Source links provide a mapping between file in documentation and code repositry (usual)." +
      |Accepted formats:
      |<sub-path>=<source-link>
      |<source-link>
      |
      |where <source-link> is one of following:
      | - `github://<organization>/<repository>` (requires revision to be specified as argument for scala3doc)
      | - `gitlab://<organization>/<repository>` (requires revision to be specified as argument for scala3doc)
      | - <scaladoc-template>
      | - <template>
      |
      |<scaladoc-template> is a format for `doc-source-url` parameter scaladoc.
      |NOTE: We only supports `€{FILE_PATH_EXT}` and €{FILE_LINE} patterns
      |
      |<template> is a liqid template string that can accepts follwoing arguments:
      | - `operation`: either "view" or "edit"
      | - `path`: relative path of file to provide link to
      | - `line`: optional parameter that specify line number within a file
      |
      |
      |Template can defined only by subset of sources defined by path prefix represented by `<sub-path>`""".stripMargin

  def load(configs: Seq[String], revision: Option[String], projectRoot: Path): SourceLinks =
    // TODO ...
    val mappings = configs.map(str => str -> SourceLink.parse(str, revision))

    val errors = mappings.collect {
      case (template, Left(message)) =>
        s"'$template': $message"
    }.mkString("\n")

    if errors.nonEmpty then println(
      s"""Following templates has invalid format:
         |$errors
         |
         |$usage
         |""".stripMargin
      )

    SourceLinks(mappings.collect {case (_, Right(link)) => link}, projectRoot)

  def load(config: DocConfiguration): SourceLinks =
    load(
      config.args.sourceLinks,
      config.args.revision,
      Paths.get("").toAbsolutePath
    )