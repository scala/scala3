import cbt._
import java.io._
import java.nio.file._

class Build(val context: Context) extends Shared{
  override def dependencies = Seq(test)
  def suffix = "root"
  override def projectDirectory = dottyHome
}

class Launcher(val context: Context) extends BootstrappedCompiler{
  //override def 
  private def cwd = context.args.headOption.map(new File(_)).getOrElse{ System.err.println(""); System.exit(0); ??? }
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
  def dottyVersion = "0.1.1-20170309-e28d8ee-NIGHTLY"
  override def dependencies = Seq[Dependency]()
  override def defaultScalaVersion = "2.11.5"

  def suffix: String
  override def name: String = "dotty-"+suffix
  override def projectDirectory = dottyHome ++ ("/"++suffix)

  def dottyHome = lib.realpath(context.workingDirectory ++ "/..") // <- make sure to override this whenever that's not true
  def compilerDirectory = dottyHome ++ "/compiler"
  def libraryDirectory = dottyHome ++ "/library"
  override def scalacOptions = super.scalacOptions ++ Seq(
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials"
  )

  // sub-projects
  def interfaces = new Interfaces(context)
  def compiler = new Compiler(context)
  def library = new Library(context)
  def libraryBootstrapped = new LibraryBootstrapped(context)
  def compilerBootstrapped = new CompilerBootstrapped(context)
  override def test = new CompilerBootstrappedTest(context)

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

class Interfaces(val context: Context) extends Shared with PackageDotty{
  def suffix = "interfaces"
  override def dependencies = Seq()
}

class Library(val context: Context) extends Shared{
  def suffix = "library"
  override def dependencies: Seq[Dependency] = Resolver(mavenCentral).bind(
    MavenDependency("org.scala-lang","scala-reflect","2.11.5")
  )
}
class LibraryBootstrapped(context: Context) extends Library(context) with NonBootstrappedCompiler{
  override def compileDependencies = Seq( library ) ++ dependencies
  override def dependencies: Seq[Dependency] = (
     //, interfaces )
    Resolver(mavenCentral).bind(
      MavenDependency("org.scala-lang","scala-reflect","2.11.5")
      //"me.d-d" % "scala-compiler" % "2.11.5-20170111-125332-40bdc7b65a",
      //"com.typesafe.sbt" % "sbt-interface" % "0.13.13"
    )
  )
  //override def dependencies: Seq[Dependency] = dottyLibraryMavenDependencies
}

class Compiler(val context: Context) extends Shared{
  def suffix = "compiler"
  override def dependencies: Seq[Dependency] = 
  (
    Seq( library, interfaces )
    ++
    dottyCompilerMavenDependencies
    /*
    Resolver(mavenCentral).bind(
      "me.d-d" % "scala-compiler" % "2.11.5-20170111-125332-40bdc7b65a",
      "com.typesafe.sbt" % "sbt-interface" % "0.13.13"
    )
    */
  )
}
class CompilerBootstrapped(context: Context) extends Compiler(context) with NonBootstrappedCompiler{
  // FIXME: using the bootstrapped library here fails with ClassNotFoundException: scala.Product0$class
  override def dependencies = Seq( libraryBootstrapped, interfaces ) ++ dottyCompilerMavenDependencies
  override def compileJavaFirst = true
  override def skipJava = false
}

class CompilerBootstrappedTest(val context: Context) extends BootstrappedCompiler{ outer =>
  def suffix = "compiler"
  override def projectDirectory = compilerDirectory ++ "/test"
  override def sources = Seq(
    projectDirectory ++ "/dotc",
    projectDirectory ++ "/dotty/tools",
    projectDirectory ++ "/dotty/Jars.scala",
    projectDirectory ++ "/dotty/partest"
  )
  override def compileDependencies = {
    //compileTarget.mkdirs
    Seq(
      //libraryBootstrapped,
      //BinaryDependency( Seq(target++"2"), Nil ) //Seq(compilerBootstrapped) )
    ) ++ dependencies
  }

  /*
  def partest =
    new BasicBuild(context) with BootstrappedCompiler{
      def suffix = ???
      override def projectDirectory = outer.projectDirectory ++ "/dotty/partest"
      override def dependencies = Seq(compilerBootstrapped) ++
        Resolver(mavenCentral).bind(
          "org.scala-lang.modules" %% "scala-partest" % "1.0.11"
        )
    }*/

  def jav =
    new BasicBuild(context) with BootstrappedCompiler{
      def suffix = ???
      override def sources = Seq( projectDirectory / "ContextEscapeDetection.java" )
      override def projectDirectory = outer.projectDirectory ++ "/dotty/tools"
      override def dependencies = Seq(compilerBootstrapped)
      override def skipJava = false
    }

  override def dependencies =
    Seq(  compilerBootstrapped ) ++
    Resolver(mavenCentral).bind(
      "org.scala-lang.modules" %% "scala-partest" % "1.0.11",
      "com.typesafe.sbt" % "sbt-interface" % "0.13.13",
      "org.scala-lang.modules" %% "scala-partest" % "1.0.11"
      //"com.novocode" % "junit-interface" % "0.11"
      //"me.d-d" % "scala-library" % "2.11.5-20170111-125332-40bdc7b65a"
    )

  //override def compileJavaFirst = true

  override def flatClassLoader = true // required so partest finds the compiler

  override def run = {
    System.setProperty("dotty.tests.classes.compiler",compilerBootstrapped.jar.get.toString)
    System.setProperty("dotty.tests.classes.library",libraryBootstrapped.jar.get.toString)
    System.setProperty("dotty.tests.classes.interfaces",interfaces.jar.get.toString)

    // trying to run the JUnit tests currently still fails with a bunch of exceptions
    runMain( "org.junit.runner.JUnitCore", Seq("dotc.tests") )
    // runMain( "dotty.partest.DPConsoleRunner", "-dottyJars 1 " + dotty.build.asInstanceOf[PackageJars].jar.get.toString )
  }
}
