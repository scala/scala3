package dotty
package tools
package vulpix

import scala.language.unsafeNulls

import java.io.File

object TestConfiguration {

  val pageWidth = 120

  val noCheckOptions = Array(
    "-pagewidth", pageWidth.toString,
    "-color:never",
    "-Xtarget", defaultTarget
  )

  val checkOptions = Array(
    // "-Yscala2-unpickler", s"${Properties.scalaLibrary}",
    "-Yno-deep-subtypes",
    "-Yno-double-bindings",
    "-Yforce-sbt-phases",
    "-Xsemanticdb",
    "-Xverify-signatures"
  )

  val basicClasspath = mkClasspath(
    Properties.scalaLibraryTasty.toList ::: List(
    Properties.scalaLibrary,
    Properties.dottyLibrary
  ))

  val withCompilerClasspath = mkClasspath(
    Properties.scalaLibraryTasty.toList ::: List(
    Properties.scalaLibrary,
    Properties.scalaAsm,
    Properties.jlineTerminal,
    Properties.jlineReader,
    Properties.compilerInterface,
    Properties.dottyInterfaces,
    Properties.dottyLibrary,
    Properties.tastyCore,
    Properties.dottyCompiler
  ))

  lazy val withStagingClasspath =
    withCompilerClasspath + File.pathSeparator + mkClasspath(List(Properties.dottyStaging))

  lazy val withTastyInspectorClasspath =
    withCompilerClasspath + File.pathSeparator + mkClasspath(List(Properties.dottyTastyInspector))

  lazy val scalaJSClasspath = mkClasspath(List(
    Properties.scalaJSJavalib,
    Properties.scalaJSLibrary,
    Properties.dottyLibraryJS
  ))

  def mkClasspath(classpaths: List[String]): String =
    classpaths.map({ p =>
      val file = new java.io.File(p)
      assert(file.exists, s"File $p couldn't be found.")
      file.getAbsolutePath
    }).mkString(File.pathSeparator)

  val yCheckOptions = Array("-Ycheck:all")

  val commonOptions = Array("-indent") ++ checkOptions ++ noCheckOptions ++ yCheckOptions
  val noYcheckCommonOptions = Array("-indent") ++ checkOptions ++ noCheckOptions
  val defaultOptions = TestFlags(basicClasspath, commonOptions)
  val noYcheckOptions = TestFlags(basicClasspath, noYcheckCommonOptions)
  val bestEffortBaselineOptions = TestFlags(basicClasspath, noCheckOptions)
  val unindentOptions = TestFlags(basicClasspath, Array("-no-indent") ++ checkOptions ++ noCheckOptions ++ yCheckOptions)
  val withCompilerOptions =
    defaultOptions.withClasspath(withCompilerClasspath).withRunClasspath(withCompilerClasspath)
  lazy val withStagingOptions =
    defaultOptions.withClasspath(withStagingClasspath).withRunClasspath(withStagingClasspath)
  lazy val withTastyInspectorOptions =
    defaultOptions.withClasspath(withTastyInspectorClasspath).withRunClasspath(withTastyInspectorClasspath)
  lazy val scalaJSOptions =
    defaultOptions.and("-scalajs").withClasspath(scalaJSClasspath).withRunClasspath(scalaJSClasspath)
  val allowDeepSubtypes = defaultOptions without "-Yno-deep-subtypes"
  val allowDoubleBindings = defaultOptions without "-Yno-double-bindings"
  val picklingOptions = defaultOptions and (
    "-Xprint-types",
    "-Ytest-pickler",
    "-Yprint-pos",
    "-Yprint-pos-syms"
  )
  val picklingWithCompilerOptions =
    picklingOptions.withClasspath(withCompilerClasspath).withRunClasspath(withCompilerClasspath)

  val explicitNullsOptions = defaultOptions and "-Yexplicit-nulls"

  /** Default target of the generated class files */
  private def defaultTarget: String = {
    import scala.util.Properties.isJavaAtLeast

    if isJavaAtLeast("9") then "9" else "8"
  }
}
