package dotty.tools.sbtplugin

/** The HTTP User-Agent used by coursier when fetching dependencies.
 *
 *  On the CI, `project/scripts/sbt` sets it as a system property of the sbt JVM, so that Maven Central
 *  can identify our traffic instead of rate-limiting it (HTTP 429) as generic.
 *  See https://central.sonatype.org/faq/429-error/
 *
 *  System properties are not inherited by the JVMs we fork (tests, scripted, coursier.jar),
 *  so they need to be forwarded explicitly with `javaOptions`.
 */
object CoursierUserAgent {
  private val properties = Seq(
    "coursier.http.agent",
    // The name of the property as read by sbt's shaded coursier (lm-coursier-shaded >= 2.1.14)
    "lmcoursier.internal.shaded.coursier.http.agent",
  )

  /** The JVM options setting the user agent, if it is set for the current JVM */
  def javaOptions: Seq[String] =
    for {
      prop <- properties
      value <- sys.props.get(prop)
    } yield s"-D$prop=$value"
}
