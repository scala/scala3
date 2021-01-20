package dotty.dokka

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex
import dotty.dokka.test.BuildInfo
import java.nio.file.Path;
import org.jsoup.Jsoup

class JavadocExternalLocationProviderIntegrationTest extends ExternalLocationProviderIntegrationTest(
  "externalJavadoc",
  List(".*java.*::javadoc::https://docs.oracle.com/javase/8/docs/api/"),
  List(
    "https://docs.oracle.com/javase/8/docs/api/java/util/stream/Stream.Builder.html",
    "https://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html",
    "https://docs.oracle.com/javase/8/docs/api/java/util/Map.html"
  )
)

class ScaladocExternalLocationProviderIntegrationTest extends ExternalLocationProviderIntegrationTest(
  "externalScaladoc",
  List(".*scala.*::scaladoc::https://www.scala-lang.org/api/current/"),
  List(
    "https://www.scala-lang.org/api/current/scala/util/matching/Regex$$Match.html",
    "https://www.scala-lang.org/api/current/scala/Predef$.html",
    "https://www.scala-lang.org/api/current/scala/collection/immutable/Map.html"
  )
)

class Scala3docExternalLocationProviderIntegrationTest extends ExternalLocationProviderIntegrationTest(
  "externalScala3doc",
  List(".*scala.*::scala3doc::https://dotty.epfl.ch/api/"),
  List(
    "https://dotty.epfl.ch/api/scala/collection/immutable/Map.html",
    "https://dotty.epfl.ch/api/scala/Predef$.html",
    "https://dotty.epfl.ch/api/scala/util/matching/Regex$$Match.html"
  )
)


abstract class ExternalLocationProviderIntegrationTest(
  name: String,
  mappings: Seq[String],
  expectedLinks: Seq[String]
  ) extends ScaladocTest(name):

  override def args = super.args.copy(
    externalMappings = mappings.flatMap( s =>
          ExternalDocLink.parse(s).fold(left => None, right => Some(right)
        )
      ).toList
  )

  override def runTest = afterRendering {
    val output = summon[DocContext].args.output.toPath.resolve("api")
    val linksBuilder = List.newBuilder[String]

    def processFile(path: Path): Unit =
      val document = Jsoup.parse(IO.read(path))
      val content = document.select(".documentableElement").forEach { elem =>
        val hrefValues = elem.select("a").asScala.map { a =>
          a.attr("href")
        }
        linksBuilder ++= hrefValues
      }


    IO.foreachFileIn(output, processFile)
    val links = linksBuilder.result
    val errors = expectedLinks.flatMap(expect => Option.when(!links.contains(expect))(expect))
    if !errors.isEmpty then {
      val reportMessage =
      "External location provider integration test failed.\n" +
      "Missing links:\n"
      + errors.mkString("\n","\n","\n")
      + "Found links:" + links.mkString("\n","\n","\n")
      reportError(reportMessage)
    }
  } :: Nil

