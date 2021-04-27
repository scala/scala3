package dotty.tools.scaladoc

import java.net.URL
import scala.util.matching._
import scala.util.{ Try, Success, Failure }

case class ExternalDocLink(
  originRegexes: List[Regex],
  documentationUrl: URL,
  kind: DocumentationKind,
  packageListUrl: Option[URL] = None
):
  def withPackageList(url: URL): ExternalDocLink = copy(packageListUrl = Some(url))

enum DocumentationKind:
  case Javadoc extends DocumentationKind
  case Scaladoc2 extends DocumentationKind
  case Scaladoc3 extends DocumentationKind

object ExternalDocLink:
  def parse(mapping: String): Either[String, ExternalDocLink] =
    def fail(msg: String) = Left(s"Unable to process external mapping $mapping. $msg")

    def tryParse[T](descr: String)(op: => T): Either[String, T] = Try(op) match {
      case Success(v) => Right(v)
      case Failure(e) => fail(s"Unable to parse $descr. Exception $e occured")
    }

    def parsePackageList(elements: List[String]) = elements match
     case List(urlStr) => tryParse("packageList")(Some(URL(urlStr)))
     case Nil => Right(None)
     case other => fail(s"Provided multiple package lists: $other")

    def doctoolByName(name: String) = name match
      case "javadoc" => Right(DocumentationKind.Javadoc)
      case "scaladoc2" => Right(DocumentationKind.Scaladoc2)
      case "scaladoc3" => Right(DocumentationKind.Scaladoc3)
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