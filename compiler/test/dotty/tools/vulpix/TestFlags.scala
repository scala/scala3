package dotty.tools.vulpix

import dotty.tools.io.ClassPath

import java.io.File as JFile

final case class TestFlags(
  defaultClassPath: String,
  runClassPath: String, // class path that is used when running `run` tests (not compiling)
  options: Array[String],
  javacOptions: Array[String]) {

  def and(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options ++ flags, javacOptions)

  def without(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options diff flags, javacOptions)

  def withClasspath(classPath: String): TestFlags =
    TestFlags(s"$defaultClassPath${ClassPath.pathSeparator}$classPath", runClassPath, options, javacOptions)

  def withRunClasspath(classPath: String): TestFlags =
    TestFlags(defaultClassPath, s"$runClassPath${ClassPath.pathSeparator}$classPath", options, javacOptions)

  def withJavacOnlyOptions(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options, javacOptions ++ flags)

  def all: Array[String] = Array("-classpath", defaultClassPath) ++ options

  def withoutLanguageFeatures: TestFlags = copy(options = withoutLanguageFeaturesOptions)

  private val languageFeatureFlag = "-language:"
  private def withoutLanguageFeaturesOptions = options.filterNot(_.startsWith(languageFeatureFlag))

  def andLanguageFeature(feature: String) =
    copy(options = options ++ Array(s"$languageFeatureFlag$feature"))

  def withoutLanguageFeature(feature: String) =
    val (languageFeatures, rest) = options.partition(_.startsWith(languageFeatureFlag))
    val existingFeatures = languageFeatures.flatMap(_.stripPrefix(languageFeatureFlag).split(","))
    val filteredFeatures = existingFeatures.filterNot(_ == feature)
    val newOptions =
      if filteredFeatures.isEmpty then rest
      else rest ++ Array(languageFeatureFlag + filteredFeatures.mkString(","))
    copy(options = newOptions)

  /** Subset of the flags that should be passed to javac. */
  def javacFlags: Array[String] = {
    val flags = all
    val cp = flags.dropWhile(_ != "-classpath").take(2)
    val output = flags.dropWhile(_ != "-d").take(2)
    cp ++ output ++ javacOptions
  }
}

object TestFlags {
  def apply(classPath: String, flags: Array[String]): TestFlags = TestFlags(classPath, classPath, flags, Array.empty)
}
