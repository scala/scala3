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
  private def fail(mapping: String, msg: String) = Left(s"Unable to process external mapping $mapping. $msg")

  private def tryParse[T](mapping: String, descr: String)(op: => T): Either[String, T] = Try(op) match {
    case Success(v) => Right(v)
    case Failure(e) => fail(mapping, s"Unable to parse $descr. Exception $e occured")
  }

  def parseLegacy(mapping: String): Either[String, ExternalDocLink] =
    mapping.split("#").toList match
      case path :: apiUrl :: Nil => for {
        url <- tryParse(mapping, "url")(URL(apiUrl))
      } yield ExternalDocLink(
        List(s"${Regex.quote(path)}.*".r),
        url,
        DocumentationKind.Scaladoc2,
        None
      )
      case _ => fail(mapping, "Wrong format of legacy external mapping. path#apiUrl format is accepted.")

  def parse(mapping: String): Either[String, ExternalDocLink] =

    def parsePackageList(elements: List[String]) = elements match
     case List(urlStr) => tryParse(mapping, "packageList")(Some(URL(urlStr)))
     case Nil => Right(None)
     case other => fail(mapping, s"Provided multiple package lists: $other")

    def doctoolByName(name: String) = name match
      case "javadoc" => Right(DocumentationKind.Javadoc)
      case "scaladoc2" => Right(DocumentationKind.Scaladoc2)
      case "scaladoc3" => Right(DocumentationKind.Scaladoc3)
      case other => fail(mapping, s"Unknown doctool: $other")


    mapping.split("::").toList match
      case regexStr :: docToolStr :: urlStr :: rest =>
        for {
          regex <- tryParse(mapping, "regex")(regexStr.r)
          url <- tryParse(mapping, "url")(URL(urlStr))
          doctool <- doctoolByName(docToolStr)
          packageList <- parsePackageList(rest)
        } yield ExternalDocLink(
            List(regex),
            url,
            doctool,
            packageList
          )
      case _ =>
        fail(mapping, "Accepted format: `regexStr::docToolStr::urlStr[::rest]`")
