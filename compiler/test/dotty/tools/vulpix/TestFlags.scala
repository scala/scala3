package dotty.tools.vulpix

import java.io.{File => JFile}

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
    TestFlags(s"$defaultClassPath${JFile.pathSeparator}$classPath", runClassPath, options, javacOptions)

  def withRunClasspath(classPath: String): TestFlags =
    TestFlags(defaultClassPath, s"$runClassPath${JFile.pathSeparator}$classPath", options, javacOptions)

  def withJavacOnlyOptions(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options, javacOptions ++ flags)

  def all: Array[String] = Array("-classpath", defaultClassPath) ++ options

  def withoutLanguageFeatures: TestFlags = copy(options = withoutLanguageFeaturesOptions)

  private val languageFeatureFlag = "-language:"
  private def withoutLanguageFeaturesOptions = options.filterNot(_.startsWith(languageFeatureFlag))

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
