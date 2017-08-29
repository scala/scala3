package dotty
package tools
package vulpix

object TestConfiguration {
  implicit val defaultOutputDir: String = "../out/"

  val noCheckOptions = Array(
    "-pagewidth", "120",
    "-color:never"
  )

  val checkOptions = Array(
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-Yforce-sbt-phases"
  )

  val classPath = mkClassPath(Jars.dottyTestDeps)

  def mkClassPath(classPaths: List[String]): String = {
    classPaths map { p =>
      val file = new java.io.File(p)
      assert(
        file.exists,
        s"""|File "$p" couldn't be found. Run `packageAll` from build tool before
            |testing.
            |
            |If running without sbt, test paths need to be setup environment variables:
            |
            | - DOTTY_LIBRARY
            | - DOTTY_COMPILER
            | - DOTTY_INTERFACES
            | - DOTTY_EXTRAS
            |
            |Where these all contain locations, except extras which is a colon
            |separated list of jars.
            |
            |When compiling with eclipse, you need the sbt-interfaces jar, put
            |it in extras."""
      )
      file.getAbsolutePath
    } mkString(":")
  }

  val yCheckOptions = Array("-Ycheck:tailrec,resolveSuper,erasure,mixin,getClass,restoreScopes,labelDef")

  val basicDefaultOptions = checkOptions ++ noCheckOptions ++ yCheckOptions
  val defaultUnoptimised = TestFlags(classPath, basicDefaultOptions)
  val defaultOptimised = defaultUnoptimised and "-optimise"
  val defaultOptions = defaultUnoptimised
  val allowDeepSubtypes = defaultOptions without "-Yno-deep-subtypes"
  val allowDoubleBindings = defaultOptions without "-Yno-double-bindings"
  val picklingOptions = defaultUnoptimised and (
    "-Xprint-types",
    "-Ytest-pickler",
    "-Yprintpos"
  )
  val picklingOptionsAllowDeepSubTypes = picklingOptions without "-Yno-deep-subtypes"
  val scala2Mode = defaultOptions and "-language:Scala2"
  val explicitUTF8 = defaultOptions and ("-encoding", "UTF8")
  val explicitUTF16 = defaultOptions and ("-encoding", "UTF16")
}
