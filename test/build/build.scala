import cbt._
import java.io._
import java.nio.file._
class Build(val context: Context) extends Dotty{
  override def dottyDependency = DirectoryDependency( projectDirectory ++ "/../compiler-bootstrapped" )
  override def sources = Seq(
    projectDirectory ++ "/../compiler/test"
  )
  def dotty = DirectoryDependency( projectDirectory ++ "/.." )
  def dottyCompiler = DirectoryDependency( projectDirectory ++ "/../compiler-bootstrapped" )
  def dottyLibrary = DirectoryDependency( projectDirectory ++ "/../library-bootstrapped" )
  def dottyInterfaces = DirectoryDependency( projectDirectory ++ "/../interfaces" )

  private def jar(d: DirectoryDependency) = d.build.asInstanceOf[PackageJars].jar.get.toString

  override def dependencies =
    Seq(
      dottyCompiler
    ) ++
    Resolver(mavenCentral).bind(
      "org.scala-lang.modules" %% "scala-partest" % "1.0.11",
      "me.d-d" % "scala-compiler" % "2.11.5-20170111-125332-40bdc7b65a"
      //"me.d-d" % "scala-library" % "2.11.5-20170111-125332-40bdc7b65a"
    )

  override def run = {
    println(new java.io.File(".").getAbsolutePath)

    System.setProperty("dotty.tests.classes.compiler",jar(dottyCompiler))
    System.setProperty("dotty.tests.classes.library",jar(dottyLibrary))
    System.setProperty("dotty.tests.classes.interfaces",jar(dottyInterfaces))

    // trying to run the JUnit tests currently still fails with a bunch of exceptions
    lib.runMain(
      "org.junit.runner.JUnitCore",
      Seq(
        "dotc.tests"
      ),
      new java.net.URLClassLoader(
        classpath.strings.map(s => new java.net.URL("file://"+s)).to
      )
    )
    /*
    lib.runMain(
      "dotty.partest.DPConsoleRunner",
      Seq(
        "-dottyJars 1 " + dotty.build.asInstanceOf[PackageJars].jar.get.toString
      ),
      new java.net.URLClassLoader(
        classpath.strings.map(s => new java.net.URL("file://"+s)).to
      )
    )
    */
  }
}
