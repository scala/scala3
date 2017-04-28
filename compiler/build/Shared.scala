import cbt._

trait Shared extends PackageJars{
  def groupId: String = "ch.epfl.lamp"
  def version: String = "0.1.1-SNAPSHOT"

  // basic cbt config
  override def dependencies = Seq[Dependency]()
  override def defaultScalaVersion = "2.11.5"

  def dottyHome = lib.realpath(context.workingDirectory ++ "/..") // <- make sure to override this whenever that's not true

  override def scalacOptions = super.scalacOptions ++ Seq(
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials"
  )

  // sub-projects
  def interfaces = new Interfaces(context)
  def libraryNonBootstrapped = new LibraryNonBootstrapped(context)
  def compilerNonBootstrapped = new CompilerNonBootstrapped(context)
  def libraryBootstrapped = new LibraryBootstrapped(context)
  def compilerBootstrapped = new CompilerBootstrapped(context)
  def compilerBootstrappedTest = new CompilerBootstrappedTest(context)

  def extraDottyCompilerDependencies =
    Resolver( mavenCentral ).bind(
      ScalaDependency( "org.scala-lang.modules", "scala-xml", "1.0.1" ),
      MavenDependency( "com.typesafe.sbt", "sbt-interface", "0.13.13" ),
      MavenDependency( "org.scala-lang.modules", "scala-asm", "5.1.0-scala-2" )
    )

  def extraDottySources = {
    val backendCheckout = GitDependency.checkout(
      "https://github.com/lampepfl/scala.git", "7246c2bd1e98fadd3a2de1bbb70403fdb8896458"
    )

    val backendDir = backendCheckout / "src" / "compiler" / "scala" / "tools" / "nsc" / "backend"

    Seq( backendDir, backendDir / "jvm" )
      .flatMap( _.listFiles )
      .filter( file =>
        file.string.endsWith(".scala") && !Seq(
          "JavaPlatform.scala", "Platform.scala", "ScalaPrimitives.scala",
          "BCodeICodeCommon.scala", "GenASM.scala", "GenBCode.scala", "ScalacBackendInterface.scala"
        ).contains(file.getName)
      )
  }
}