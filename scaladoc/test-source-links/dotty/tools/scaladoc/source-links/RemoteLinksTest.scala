package dotty.tools.scaladoc
package sourcelinks

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.tools.scaladoc.test.BuildInfo
import java.nio.file.Path
import java.nio.file.Paths
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import util.IO
import org.junit.Assert.assertTrue
import org.junit.Test

class RemoteLinksTest:

  @Test
  def runTest =
    val mtsl = membersToSourceLinks(using testDocContext())
    val pageToMtsl: Map[String, List[(String, String)]] = mtsl.groupMap(_._2.split("#L").head)(v => (v._1, v._2.split("#L").last))
    pageToMtsl.foreach { case (link, members) =>
      try
        val doc = getDocumentFromUrl(link)
        members.foreach { (member, line) =>
          if !member.startsWith("given_") then // TODO: handle synthetic givens, for now we disable them from testing
            val loc = doc.select(s"#LC$line").text
            val memberToMatch = member.replace("`", "")
            assertTrue(s"Expected to find $memberToMatch at $link at line $line", loc.contains(memberToMatch))
        }
      catch
        case e: java.lang.IllegalArgumentException =>
          report.error(s"Could not open link for $link - invalid URL")(using testContext)
        case e: org.jsoup.HttpStatusException => e.getStatusCode match
          case 404 => throw AssertionError(s"Page $link does not exists")
          case n   => report.warning(s"Could not open link for $link, return code $n")(using testContext)
    }
    assertNoErrors(testContext.reportedDiagnostics)

  private def getDocumentFromUrl(link: String): Document =
    try
      Jsoup.connect(link).get
    catch
      case e: org.jsoup.HttpStatusException => e.getStatusCode match
          case 429 =>
            Thread.sleep(10)
            getDocumentFromUrl(link)
          case n =>
            throw e

  private def membersToSourceLinks(using DocContext): List[(String, String)] =
    val output = Paths.get("scaladoc", "output", "scala3", "api").toAbsolutePath
    val mtsl = List.newBuilder[(String, String)]
    def processFile(path: Path): Unit =
      val document = Jsoup.parse(IO.read(path))
      if document.select("span.kind").first.text == "package" then
        document.select(".documentableElement").forEach { dElem =>
          if dElem.select("span.kind").first.text != "package" then
            dElem.select("dt").forEach { elem =>
              val content = elem.text
              if content == "Source" then
                mtsl += dElem.select(".documentableName").first.text -> elem.nextSibling.childNode(0).attr("href")
            }
        }
    IO.foreachFileIn(output, processFile)
    mtsl.result
