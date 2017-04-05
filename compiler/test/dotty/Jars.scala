package dotty

/** Jars used when compiling test, normally set from the sbt build */
object Jars {
  val dottyLib: String = sys.env.get("DOTTY_LIB")
    .getOrElse(sys.props("dotty.tests.classes.library"))

  val dottyCompiler: String = sys.env.get("DOTTY_COMPILER")
    .getOrElse(sys.props("dotty.tests.classes.compiler"))

  val dottyInterfaces: String = sys.env.get("DOTTY_INTERFACE")
    .getOrElse(sys.props("dotty.tests.classes.interfaces"))

  val dottyExtras: List[String] = Option(sys.env.get("DOTTY_EXTRAS")
    .getOrElse(sys.props("dotty.tests.extraclasspath")))
    .map(_.split(":").toList).getOrElse(Nil)

  val dottyReplDeps: List[String] = dottyLib :: dottyExtras

  val dottyTestDeps: List[String] =
    dottyLib :: dottyCompiler :: dottyInterfaces :: dottyExtras


  def scalaLibraryFromRuntime: String = findJarFromRuntime("scala-library-2.")

  private def findJarFromRuntime(partialName: String) = {
    val urls = ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile.toString)
    urls.find(_.contains(partialName)).getOrElse {
      throw new java.io.FileNotFoundException(
        s"""Unable to locate $partialName on classpath:\n${urls.toList.mkString("\n")}"""
      )
    }
  }

}
