package dotty.dokka

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.dokka.test.BuildInfo

import org.jetbrains.dokka.pages.{RootPageNode, PageNode, ContentPage, ContentText, ContentNode, ContentComposite}

import dotty.dokka.model.api._

private enum Signature:
  case Expected(name: String, signature: String)
  case Unexpected(name: String)
import Signature._

abstract class SignatureTest(
  testName: String,
  signatureKinds: Seq[String],
  sourceFiles: List[String] = Nil,
  ignoreMissingSignatures: Boolean = false,
  filterFunc: (Member) => Boolean = _ => true
) extends ScaladocTest(testName):
  override def assertions = Assertion.AfterPagesTransformation { root =>
    val sources = sourceFiles match
      case Nil => testName :: Nil
      case s => s

    val allSignaturesFromSources = sources
      .map { file => Source.fromFile(s"${BuildInfo.test_testcasesSourceRoot}/tests/$file.scala") }
      .flatMap(signaturesFromSources(_, signatureKinds))
      .toList
    val expectedFromSources: Map[String, List[String]] = allSignaturesFromSources
      .collect { case e: Expected => e }
      .groupMap(_.name)(_.signature)
    val unexpectedFromSources: Set[String] = allSignaturesFromSources.collect { case Unexpected(name) => name }.toSet

    val actualSignatures: Map[String, Seq[String]] = signaturesFromDocumentation(root).flatMap { signature =>
      findName(signature, signatureKinds).map(_ -> signature)
    }.groupMap(_._1)(_._2)

    val unexpected = unexpectedFromSources.flatMap(actualSignatures.get)
    val expectedButNotFound = expectedFromSources.flatMap {
      case (k, v) => findMissingSingatures(v, actualSignatures.getOrElse(k, Nil))
    }

    val missingReport = Option.when(!ignoreMissingSignatures && !expectedButNotFound.isEmpty)
      (s"Not documented signatures:\n${expectedButNotFound.mkString("\n")}")
    val unexpectedReport = Option.when(!unexpected.isEmpty)
      (s"Unexpectedly documented signatures:\n${unexpected.mkString("\n")}")
    val reports = missingReport ++ unexpectedReport

    if !reports.isEmpty then
      val allSignaturesMessage =
        s"""
        |All documented signatures:
        |${actualSignatures.flatMap(_._2).mkString("\n")}
        |
        |All expected signatures from source:
        |${expectedFromSources.flatMap(_._2).mkString("\n")}
        """.stripMargin
      val errorMessage = (reports ++ Some(allSignaturesMessage)).mkString(start = "\n", sep = "\n\n", end = "\n")
      reportError(errorMessage)
    end if

  } :: Nil

  // e.g. to remove '(0)' from object IAmACaseObject extends CaseImplementThis/*<-*/(0)/*->*/
  private val commentRegex = raw"\/\*<-\*\/[^\/]+\/\*->\*\/".r
  private val whitespaceRegex = raw"\s+".r
  private val expectedRegex = raw".+//expected: (.+)".r
  private val unexpectedRegex = raw"(.+)//unexpected".r
  private val identifierRegex = raw"^\s*(`.*`|(?:\w+)(?:_[^\[\(\s]+)|\w+|[^\[\(\s]+)".r

  private def findMissingSingatures(expected: Seq[String], actual: Seq[String]): Set[String] =
    expected.toSet &~ actual.toSet

  extension (s: String):
    private def startWithAnyOfThese(c: String*) = c.exists(s.startsWith)
    private def compactWhitespaces = whitespaceRegex.replaceAllIn(s, " ")

  private def findName(signature: String, kinds: Seq[String]): Option[String] =
    for
      kindMatch <- kinds.flatMap(k => s"\\b$k\\b".r.findFirstMatchIn(signature)).headOption
      afterKind <- Option(kindMatch.after(0)) // to filter out nulls
      nameMatch <- identifierRegex.findFirstMatchIn(afterKind)
    yield nameMatch.group(1)

  private def signaturesFromSources(source: Source, kinds: Seq[String]): Seq[Signature] =
    source.getLines.map(_.trim)
        .filterNot(_.isEmpty)
        .filterNot(_.startWithAnyOfThese("=",":","{","}", "//"))
        .toSeq
        .flatMap {
          case unexpectedRegex(signature) => findName(signature, kinds).map(Unexpected(_))
          case expectedRegex(signature) => findName(signature, kinds).map(Expected(_, signature))
          case signature =>
            findName(signature, kinds).map(Expected(_, commentRegex.replaceAllIn(signature, "").compactWhitespaces))
        }

  private def signaturesFromDocumentation(root: PageNode): Seq[String] =
    def flattenToText(node: ContentNode) : Seq[String] = node match
      case t: ContentText => Seq(t.getText)
      case c: ContentComposite =>
          c.getChildren.asScala.flatMap(flattenToText).toSeq
      case l: DocumentableElement =>
          (l.annotations ++ Seq(" ") ++ l.modifiers ++ Seq(l.nameWithStyles.name) ++ l.signature).map {
              case s: String => s
              case Link(s: String, _) => s
          }
      case _ => Seq()

    def all(p: ContentNode => Boolean)(n: ContentNode): Seq[ContentNode] =
        if p(n) then Seq(n) else n.getChildren.asScala.toSeq.flatMap(all(p))

    extension (page: PageNode) def allPages: List[PageNode] = page :: page.getChildren.asScala.toList.flatMap(_.allPages)

    val nodes = root.allPages
      .collect { case p: ContentPage => p }
      .filter( p => Option(p.getDocumentable).map(filterFunc).getOrElse(true))
      .flatMap(p => all(_.isInstanceOf[DocumentableElement])(p.getContent))
    nodes.map(flattenToText(_).mkString.compactWhitespaces.trim)

object SignatureTest {
  val classlikeKinds = Seq("class",  "object", "trait", "enum") // TODO add docs for packages
  val members = Seq("type", "def", "val", "var")
  val all = classlikeKinds ++ members
}
