package dotty.tools.vulpix

final case class TestFlags(
  defaultClassPath: String,
  runClassPath: String, // class path that is used when running `run` tests (not compiling)
  options: Array[String]) {

  def and(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options ++ flags)

  def without(flags: String*): TestFlags =
    TestFlags(defaultClassPath, runClassPath, options diff flags)

  def all: Array[String] = Array("-classpath", defaultClassPath) ++ options
}

object TestFlags {
  def apply(classPath: String, flags: Array[String]): TestFlags = TestFlags(classPath, classPath, flags)
}
