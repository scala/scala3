package dotty.tools.scaladoc
package sourcelinks

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.tools.scaladoc.test.BuildInfo
import java.nio.file.Path;
import org.jsoup.Jsoup
import util.IO
import org.junit.Assert.assertTrue

class RemoteLinksTest extends ScaladocTest(""): // empty since it is unused in our case

  def runTest = afterRendering {
    val mtsl = membersToSourceLinks
    val pageToMtsl: Map[String, List[(String, String)]] = mtsl.groupMap(_._2.split("#L").head)(v => (v._1, v._2.split("#L").last))
    pageToMtsl.foreach { case (link, members) =>
      try
        val doc = Jsoup.connect(link).get
        members.foreach { (member, line) =>
          if !member.startsWith("given_") then // TODO: handle synthetic givens, for now we disable them from testing
            val loc = doc.select(s"#LC$line").text
            assertTrue(s"Expected to find $member at $link at line $line", loc.contains(member))
        }
      catch
        case e: org.jsoup.HttpStatusException => e.getStatusCode match
          case 404 => throw AssertionError(s"Page $link does not exists")
          case n   => report.warning(s"Could not open link for $link, return code $n")
    }
  }

  private def membersToSourceLinks(using DocContext): List[(String, String)] =
    val output = summon[DocContext].args.output.toPath.resolve("api")
    val mtsl = List.newBuilder[(String, String)]
    def processFile(path: Path): Unit =
      val document = Jsoup.parse(IO.read(path))
      document.select(".documentableElement").remove()
      document.select("dt").forEach { elem =>
        val content = elem.text
        if content == "Source" then
          mtsl += document.select("h1").first.text -> elem.nextSibling.childNode(0).attr("href")
      }
    IO.foreachFileIn(output, processFile)
    mtsl.result


