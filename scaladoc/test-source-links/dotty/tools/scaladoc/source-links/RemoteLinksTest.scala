package dotty.tools.scaladoc
package sourcelinks

import scala.util.Random
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

  class TimeoutException extends Exception

  val randomGenerator = new Random(125L)
  // Predef has often problems with positions
  val mtslAll = membersToSourceLinks(using testDocContext()).filter(_._1 != "Predef")

  @Test
  def scala213XSourceLink =
    assertTrue(mtslAll.find((k, _) => k == "AbstractMap").isDefined) // source link to Scala2.13.X stdlib class

  @Test
  def scala3SourceLink =
    assertTrue(mtslAll.find((k, _) => k == "PolyFunction").isDefined) // source link to Scala3 stdlib class

  @Test
  def tastySourceLink =
    assertTrue(mtslAll.find((k, _) => k == "TastyBuffer").isDefined) // source link to Scala3 tasty class

  @Test
  def runTest =
    assertTrue(mtslAll.nonEmpty)
    val mtsl = randomGenerator.shuffle(mtslAll).take(80) // take 80 random entries
    val pageToMtsl: Map[String, List[(String, Int)]] =
      mtsl.groupMap(_._2.split("#L").head)(v => (v._1, v._2.split("#L").last.toInt))
    pageToMtsl.toSeq.sortBy(_._1).foreach { case (link, members) =>
      try
        val doc = getDocumentFromUrl(link)
        println(s"Checking $link")
        members.foreach { case (member, expectedLine) =>
          if !member.startsWith("given_") then // TODO: handle synthetic givens, for now we disable them from testing
            val toLine = expectedLine + 3
            val memberToMatch = member.replace("`", "")
            val lineCorrectlyDefined = (expectedLine until toLine).exists{ line =>
              val loc = doc.select(s"#LC$line").text

              loc.contains(memberToMatch)
            }

            assertTrue(s"Expected to find $memberToMatch at $link at lines $expectedLine-$toLine", lineCorrectlyDefined)
        }
      catch
        case e: java.lang.IllegalArgumentException =>
          report.error(s"Could not open link for $link - invalid URL")(using testContext)
        case e: TimeoutException =>
          report.error(s"Tried to open link $link 16 times but with no avail")(using testContext)
        case e: org.jsoup.HttpStatusException => e.getStatusCode match
          case 404 => throw AssertionError(s"Page $link does not exists")
          case n   => report.warning(s"Could not open link for $link, return code $n")(using testContext)
    }
    assertNoErrors(testContext.reportedDiagnostics)

  private def getDocumentFromUrl(link: String, retries: Int = 16): Document =
    try
      if retries == 0 then throw TimeoutException()
      Jsoup.connect(link).get
    catch
      case e: org.jsoup.HttpStatusException => e.getStatusCode match
          case 429 =>
            Thread.sleep(10)
            getDocumentFromUrl(link, retries - 1)
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
