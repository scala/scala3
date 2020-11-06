package dotty.tools.vulpix

import java.io.{File => JFile}

final case class TestFlags(
  defaultClassPath: String,
  runClassPath: String, // class path that is used when running `run` tests (not compiling)
  options: Array[String]) {

  def and(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options ++ flags)

  def without(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options diff flags)

  def withClasspath(classPath: String): TestFlags =
    TestFlags(s"$defaultClassPath${JFile.pathSeparator}$classPath", runClassPath, options)

  def withRunClasspath(classPath: String): TestFlags =
    TestFlags(defaultClassPath, s"$runClassPath${JFile.pathSeparator}$classPath", options)

  def all: Array[String] = Array("-classpath", defaultClassPath) ++ options

  def withoutLanguageFeatures: TestFlags = copy(options = withoutLanguageFeaturesOptions)

  private val languageFeatureFlag = "-language:"
  private def withoutLanguageFeaturesOptions = options.filterNot(_.startsWith(languageFeatureFlag))

  // TODO simplify to add `-language:feature` to `options` once
  //      https://github.com/lampepfl/dotty/issues/9787 is implemented
  def andLanguageFeature(feature: String) =
    val (languageFeatures, rest) = options.partition(_.startsWith(languageFeatureFlag))
    val existingFeatures = languageFeatures.flatMap(_.stripPrefix(languageFeatureFlag).split(","))
    val featurePrefix =
      if existingFeatures.isEmpty then ""
      else existingFeatures.mkString(",") + ","
    copy(options = rest ++ Array(languageFeatureFlag + featurePrefix + feature))

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
    cp ++ output
  }
}

object TestFlags {
  def apply(classPath: String, flags: Array[String]): TestFlags = TestFlags(classPath, classPath, flags)
}
