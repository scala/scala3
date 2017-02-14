import cbt._
import java.io._
import java.nio.file._

class Build(val context: Context) extends BootstrappedCompiler{
  private def cwd = new File(context.args.head)
  private def args = context.args.drop(1)
  override def sources = args.map(f => cwd ++ ("/"++f))
  override def compileTarget = cwd
  override def name: String = "dotty"
  override def projectDirectory = dottyHome
  def suffix = ???
}

trait PackageDotty extends cbt.PackageJars{
  def groupId: String = "ch.epfl.lamp"
  def name: String
  def version: String = Dotty.version
}

trait NonBootstrappedCompiler extends Shared with cbt.Dotty with PackageDotty{
  override def target = lib.realpath( super.target ++ "/../bootstrapped/target" )
  override def dottyVersion = super.dottyVersion
  override def dottyCompiler = compiler
  override def dottyLibrary = library // library used for compiled artifact
}

trait BootstrappedCompiler extends Shared with cbt.Dotty{
  override def dottyVersion = super.dottyVersion
  override def dottyCompiler = compilerBootstrapped
  override def dottyLibrary = libraryBootstrapped
}

trait Shared extends BaseBuild{
  // basic cbt config
  def dottyVersion = Dotty.version
  override def dependencies = Seq[Dependency]()
  override def defaultScalaVersion = "2.11.5"

  def suffix: String
  def name: String = "dotty-"+suffix
  override def projectDirectory = dottyHome ++ ("/"++suffix)

  def dottyHome = lib.realpath(context.workingDirectory ++ "/..") // <- make sure to override this whenever that's not true
  def compilerDirectory = dottyHome ++ "/compiler"
  def libraryDirectory = dottyHome ++ "/library"

  // sub-projects
  def interfaces = new Interfaces
  def compiler = new Compiler
  def library = new Library
  def libraryBootstrapped = new LibraryBootstrapped
  def compilerBootstrapped = new CompilerBootstrapped
  override def test = new CompilerBootstrappedTest

  // copy dependencies from maven ;), this way we propagate them from the sbt build
  def dottyLibraryMavenDependencies  = Resolver(mavenCentral).bindOne( Dotty.libraryOnMaven(dottyVersion) ).dependencies
  def dottyCompilerMavenDependencies = Resolver(mavenCentral).bindOne( Dotty.compilerOnMaven(dottyVersion) ).dependencies.filter{
    // ignore the things we build locally
    case d: BoundMavenDependency
      if d.mavenDependency.groupId == Dotty.groupId
      && ( d.mavenDependency.artifactId == Dotty.interfacesArtifactId
        || d.mavenDependency.artifactId == Dotty.libraryArtifactId )
      => false
    case other => true
  }
}

class Interfaces(implicit val context: Context) extends Shared with PackageDotty{
  def suffix = "interfaces"
  override def dependencies = Seq()
}

class Library(implicit val context: Context) extends Shared{
  def suffix = "library"
  override def dependencies: Seq[Dependency] = dottyLibraryMavenDependencies
}
class LibraryBootstrapped(implicit context: Context) extends Library with NonBootstrappedCompiler{
  override def dependencies: Seq[Dependency] = dottyLibraryMavenDependencies
}

class Compiler(implicit val context: Context) extends Shared{
  def suffix = "compiler"
  override def dependencies = Seq( library, interfaces ) ++ dottyCompilerMavenDependencies
}
class CompilerBootstrapped(implicit ontext: Context) extends Compiler with NonBootstrappedCompiler{
  // FIXME: using the bootstrapped library here fails with ClassNotFoundException: scala.Product0$class
  override def dependencies = Seq( libraryBootstrapped, interfaces ) ++ dottyCompilerMavenDependencies
}

class CompilerBootstrappedTest(implicit val context: Context) extends BootstrappedCompiler{
  def suffix = "compiler"
  override def projectDirectory = compilerDirectory ++ "/test"
  override def sources = Seq(
    projectDirectory ++ "/dotc",
    projectDirectory ++ "/dotty/tools",
    projectDirectory ++ "/dotty/partest",
    projectDirectory ++ "/dotty/Jars.scala"
  )
  override def dependencies =
    compilerBootstrapped +:
    Resolver(mavenCentral).bind(
      "org.scala-lang.modules" %% "scala-partest" % "1.0.11",
      "me.d-d" % "scala-compiler" % "2.11.5-20170111-125332-40bdc7b65a"
      //"me.d-d" % "scala-library" % "2.11.5-20170111-125332-40bdc7b65a"
    )

  override def compileJavaFirst = true

  override def flatClassLoader = true // required so partest finds the compiler

  override def run = {
    System.setProperty("dotty.tests.classes.compiler",compilerBootstrapped.jar.get.toString)
    System.setProperty("dotty.tests.classes.library",libraryBootstrapped.jar.get.toString)
    System.setProperty("dotty.tests.classes.interfaces",interfaces.jar.get.toString)

    // trying to run the JUnit tests currently still fails with a bunch of exceptions
    runMain( "org.junit.runner.JUnitCore", "dotc.tests" )
    // runMain( "dotty.partest.DPConsoleRunner", "-dottyJars 1 " + dotty.build.asInstanceOf[PackageJars].jar.get.toString )
  }
}
