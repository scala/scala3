package dotty

/** Jars used when compiling test, normally set from the sbt build */
object Jars {
  private def envPropOrError( environmentVariable: String, systemProperty: String ): String = {
    sys.env.get(environmentVariable) orElse
      sys.props.get(systemProperty) getOrElse (
        throw new RuntimeException(s"Please define either $environmentVariable or $systemProperty")
      )
  }

  def dottyLib: String = envPropOrError("DOTTY_LIB","dotty.tests.classes.library")
  def dottyCompiler: String = envPropOrError("DOTTY_COMPILER","dotty.tests.classes.compiler")
  def dottyInterfaces: String = envPropOrError("DOTTY_INTERFACE","dotty.tests.classes.interfaces")

  /** Dotty extras classpath from env or properties */
  val dottyExtras: List[String] = sys.env.get("DOTTY_EXTRAS")
    .map(_.split(":").toList).getOrElse(Properties.dottyExtras)

  def dottyReplDeps: List[String] = dottyLib :: dottyExtras

  def dottyTestDeps: List[String] =
    dottyLib :: dottyCompiler :: dottyInterfaces :: dottyExtras

  def scalaLibrary: String = sys.env.get("DOTTY_SCALA_LIBRARY")
    .getOrElse(findJarFromRuntime("scala-library-2."))

  /** Gets the scala 2.* library at runtime, note that doing this is unsafe
   *  unless you know that the library will be on the classpath of the running
   *  application. It is currently safe to call this function if the tests are
   *  run by sbt.
   */
  private def findJarFromRuntime(partialName: String) = {
    val urls = ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile.toString)
    urls.find(_.contains(partialName)).getOrElse {
      throw new java.io.FileNotFoundException(
        s"""Unable to locate $partialName on classpath:\n${urls.toList.mkString("\n")}"""
      )
    }
  }
}
