package dotty.tools.pc.utils

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.MtagsEnrichments.*

/**
 * Locates zip file on disk that contains the source code for the JDK.
 */
object JdkSources:

  val zipFileName = "src.zip"
  private val sources = Paths.get(zipFileName)
  private val libSources = Paths.get("lib").resolve(sources)

  def apply(
      userJavaHome: Option[String] = None
  ): Either[NoSourcesAvailable, Path] =
    val paths = candidates(userJavaHome)
    paths.find(Files.isRegularFile(_)) match
      case Some(value) => Right(value)
      case None => Left(NoSourcesAvailable(paths))

  def defaultJavaHome: Option[String] =
    Option(System.getenv("JAVA_HOME")).orElse(
      Option(System.getProperty("java.home"))
    )

  private def candidates(userJavaHome: Option[String]): List[Path] =
    def isJdkCandidate(path: Path): Boolean =
      def containsJre = path.resolve("jre").exists
      def containsRelease = path.resolve("release").exists
      val name = path.filename.toString
      name.contains("jdk") || // e.g. jdk-8, java-openjdk-11
      containsJre ||
      containsRelease

    for
      javaHomeString <- userJavaHome.orElse(defaultJavaHome).toList
      javaHome = Paths.get(javaHomeString)
      jdkHome =
        if (isJdkCandidate(javaHome)) {
          Nil
        } else
          // In case java.home points to the JRE instead of the JDK,
          // try to find jdk among its siblings
          Option(javaHome.getParent).toList
            .flatMap(Files.list(_).toList.asScala)
            .filter(isJdkCandidate)
            .toArray[Path]
            .sortBy(_.filename)
            .toList
      jdk <- jdkHome ++ List(javaHome) ++ Option(javaHome.getParent)
      src <- List(sources, libSources).map(jdk.resolve)
    yield src

  case class NoSourcesAvailable(candidates: List[Path])
