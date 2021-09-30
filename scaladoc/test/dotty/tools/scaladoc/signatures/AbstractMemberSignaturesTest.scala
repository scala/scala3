package dotty.tools.scaladoc
package signatures

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.tools.scaladoc.test.BuildInfo
import java.nio.file.Path;
import org.jsoup.Jsoup
import util.IO
import org.junit.Assert.assertTrue

class AbstractMembers extends ScaladocTest("abstractmembersignatures"):

  def runTest = {
    afterRendering {
      val actualSignatures = signaturesFromDocumentation()

      actualSignatures.foreach { (k, v) => k match
        case "Abstract methods" => assertTrue(v.forall(_._2 == "shouldBeAbstract"))
        case "Concrete methods" => assertTrue(v.forall(_._2 == "shouldBeConcrete"))
        case "Classlikes" => assertTrue(v.forall((m, n) => m.contains("abstract") == n.contains("Abstract")))
        case _ =>
      }
    }
  }

  private def signaturesFromDocumentation()(using DocContext): Map[String, List[(String, String)]] =
    val output = summon[DocContext].args.output.toPath
    val signatures = List.newBuilder[(String, (String, String))]
    def processFile(path: Path): Unit =
      val document = Jsoup.parse(IO.read(path))
      val content = document.select(".documentableList").forEach { elem =>
        val group = elem.select(".groupHeader").eachText.asScala.mkString("")
        elem.select(".documentableElement").forEach { elem =>
          val modifiers = elem.select(".header .modifiers").eachText.asScala.mkString("")
          val name = elem.select(".header .documentableName").eachText.asScala.mkString("")
          signatures += group -> (modifiers, name)
        }
      }
    IO.foreachFileIn(output, processFile)
    signatures.result.groupMap(_._1)(_._2)
