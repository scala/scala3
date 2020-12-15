package dotty.dokka

import java.net.URL
import scala.util.matching._
import scala.util.Try

case class ExternalDocLink(
  originRegexes: List[Regex],
  documentationUrl: URL,
  kind: DocumentationKind,
  packageListUrl: Option[URL] = None
):
  def withPackageList(url: URL): ExternalDocLink = copy(packageListUrl = Some(url))

enum DocumentationKind:
  case Javadoc extends DocumentationKind
  case Scaladoc extends DocumentationKind
  case Scala3doc extends DocumentationKind

object ExternalDocLink:
  def parse(mapping: String)(using CompilerContext): Option[ExternalDocLink] =
    def fail(msg: String) =
      report.warning(s"Unable to parocess external mapping $mapping. $msg")
      None

    def tryParse[T](descr: String)(op: => T): Option[T] = try Some(op) catch
      case e: RuntimeException =>
        report.warn("Unable to parse $descr", e)
        None

    def parsePackageList(elements: List[String]) = elements match
     case List(urlStr) => tryParse("packageList")(Option(URL(urlStr)))
     case Nil => Some(None)
     case other => fail(s"Provided multiple package lists: $other")

    def doctoolByName(name: String) = name match
      case "javadoc" => Some(DocumentationKind.Javadoc)
      case "scaladoc" => Some(DocumentationKind.Scaladoc)
      case "scala3doc" => Some(DocumentationKind.Scala3doc)
      case other => fail(s"Unknown doctool: $other")


    mapping.split("::").toList match
      case regexStr :: docToolStr :: urlStr :: rest =>
        for {
          regex <- tryParse("regex")(regexStr.r)
          url <- tryParse("url")(URL(urlStr))
          doctool <- doctoolByName(docToolStr)
          packageList <- parsePackageList(rest)
        } yield ExternalDocLink(
          List(regex),
          url,
          doctool,
          packageList
        )
      case _ =>
        fail("Accepted format: `regexStr::docToolStr::urlStr[::rest]`")