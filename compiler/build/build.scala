import cbt._
import java.io._
import java.nio.file._

class Build(val context: Context) extends Shared{
  override def projectDirectory = dottyHome
}

class Interfaces(val context: Context) extends Shared{
  override def name = "dotty-interfaces"
  override def projectDirectory = dottyHome / "interfaces"
  override def dependencies = Seq()
}

class LibraryNonBootstrapped(val context: Context) extends Shared{
  override def name = "dotty-library"
  override def projectDirectory = dottyHome / "library"
  override def target = projectDirectory / "target" / "non-bootstrapped"
  override def dependencies: Seq[Dependency] = Resolver( mavenCentral ).bind(
    MavenDependency("org.scala-lang","scala-reflect","2.11.5")
  )
}

class CompilerNonBootstrapped(val context: Context) extends Shared{
  override def name = "dotty-compiler"
  override def projectDirectory = dottyHome / "compiler"
  override def target = projectDirectory / "target" / "non-bootstrapped"
  override def sources = super.sources ++ extraDottySources
  override def dependencies: Seq[Dependency] = (
    libraryNonBootstrapped +:
    interfaces +:
    extraDottyCompilerDependencies
  )
}
trait UseNonBootstrappedCompiler extends Shared with cbt.CustomDotty{
  override def dottyCompiler = compilerNonBootstrapped
  override def dottyLibrary = libraryNonBootstrapped
}

class LibraryBootstrapped(val context: Context) extends UseNonBootstrappedCompiler{
  override def name = "dotty-library"
  override def projectDirectory = dottyHome / "library"
  override def target = projectDirectory / "target" / "bootstrapped"
  override def dependencies = Resolver( mavenCentral ).bind(
    MavenDependency("org.scala-lang","scala-reflect","2.11.5")
  )
}

class CompilerBootstrapped(val context: Context) extends UseNonBootstrappedCompiler{
  override def name = "dotty-compiler"
  override def projectDirectory = dottyHome / "compiler"
  override def target = projectDirectory / "target" / "bootstrapped"
  override def sources = super.sources ++ extraDottySources

  override def dependencies = (
    libraryBootstrapped +:
    interfaces +:
    extraDottyCompilerDependencies
  )

  override def compileJavaFirst = true
}

trait UseBootstrappedCompiler extends Shared with cbt.CustomDotty{
  override def dottyCompiler = compilerBootstrapped
  override def dottyLibrary = libraryBootstrapped
}

class CompilerBootstrappedTest(val context: Context) extends UseBootstrappedCompiler{ outer =>
  override def name = "dotty-compiler-tests"
  override def projectDirectory = dottyHome / "compiler" / "test"
  override def sources = Seq(
    projectDirectory ++ "/dotc",
    projectDirectory ++ "/dotty/tools",
    projectDirectory ++ "/dotty/Jars.scala",
    projectDirectory ++ "/dotty/Properties.scala"
  )
  override def dependencies = Seq(
    compilerBootstrapped
  ) ++ Resolver( mavenCentral ).bind(
    MavenDependency("junit","junit","4.11")
  )

  override def flatClassLoader = true // required so junit finds the tests

  override def run = {
    System.setProperty("dotty.tests.classes.compiler",compilerBootstrapped.jar.get.toString)
    System.setProperty("dotty.tests.classes.library",libraryBootstrapped.jar.get.toString)
    System.setProperty("dotty.tests.classes.interfaces",interfaces.jar.get.toString)

    runMain( "org.junit.runner.JUnitCore", Seq("dotc.tests") )
  }
}

/*
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
*/
