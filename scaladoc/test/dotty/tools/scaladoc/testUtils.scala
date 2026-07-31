package dotty.tools.scaladoc

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.reporting.ConsoleReporter
import dotty.tools.dotc.interfaces.Diagnostic.{ERROR, INFO, WARNING}
import dotty.tools.scaladoc.test.BuildInfo
import org.junit.Assert._
import java.io.File
import java.nio.file.{Files, Path, Paths}


case class ReportedDiagnostics(errors: List[Diagnostic], warnings: List[Diagnostic], infos: List[Diagnostic]):
  def errorMsgs = errors.map(_.msg.message)
  def warningMsgs = warnings.map(_.msg.message)
  def infoMsgs = infos.map(_.msg.message)


extension (c: CompilerContext) def reportedDiagnostics: ReportedDiagnostics =
  val t = c.reporter.asInstanceOf[TestReporter]
  ReportedDiagnostics(t.errors.result(), t.warnings.result(), t.infos.result())

def assertNoWarning(diag: ReportedDiagnostics) = assertEquals("Warnings should be empty", Nil, diag.warningMsgs)
def assertNoErrors(diag: ReportedDiagnostics) = assertEquals("Erros should be empty", Nil, diag.errorMsgs)
def assertNoInfos(diag: ReportedDiagnostics) = assertEquals("Infos should be empty", Nil, diag.infoMsgs)

def assertMessagesAbout(messages: Seq[String])(patterns: String*) =
  patterns.foldLeft(messages){ (toCheck, pattern) =>
    val (matching, rest) = toCheck.partition(_.contains(pattern))
    assertTrue(
      s"Unable to find messages matching `$pattern`in $toCheck" +
        " (not that some methods may be filtered out by previous patterns",
      matching.nonEmpty
    )
    rest
  }

class TestReporter extends ConsoleReporter:
  val errors = List.newBuilder[Diagnostic]
  val warnings = List.newBuilder[Diagnostic]
  val infos = List.newBuilder[Diagnostic]


  override def doReport(dia: Diagnostic)(using Context): Unit = dia.level match
    case INFO =>
      infos += dia
    case ERROR =>
      errors += dia
      super.doReport(dia)
    case WARNING =>
      warnings += dia
      super.doReport(dia)

def testArgs(files: Seq[File] = Nil, dest: File = new File("notUsed")) = Scaladoc.Args(
          name = "Test Project Name",
          output = dest,
          tastyFiles = files,
          docsRoot = Some(""),
        )

def testContext =
  val ctx = (new ContextBase).initialCtx.fresh.setReporter(new TestReporter)
  ctx.setSetting(ctx.settings.Yusejavacp, true)
  ctx

def testDocContext(files: Seq[File] = Nil) = DocContext(testArgs(files), testContext)

def tastyFiles(name: String, allowEmpty: Boolean = false, rootPck: String = "tests") =
  def listFilesSafe(dir: File) = Option(dir.listFiles).getOrElse {
    throw AssertionError(s"$dir not found. The test name is incorrect or scaladoc-testcases were not recompiled.")
  }
  def collectFiles(dir: File): List[File] = listFilesSafe(dir).toList.flatMap {
      case f if f.isDirectory => collectFiles(f)
      case f if f.getName.endsWith(".tasty") => f :: Nil
      case _ => Nil
    }
  val outputDir = BuildInfo.test_testcasesOutputDir
  val files = outputDir.flatMap(p => collectFiles(File(s"$p/$rootPck/$name")))
  assert(files.nonEmpty || allowEmpty)
  files.toSeq

def testDocPath: Path = Paths.get(BuildInfo.testDocumentationRoot)

/** JVM classpath of the test runner; used when compiling sources or running Scaladoc at test time. */
def javaClasspath: String = System.getProperty("java.class.path")

/** Copy a test resource `<resourceDir>/<name>.txt` into `root/sources/<name>` and return that path.
 *  Why .txt? So that the files don't conflict with other tests that load all .scala files... not great. */
def copyTestResource(root: Path, resourceDir: String, name: String): Path =
  val resource = classOf[TestReporter].getResource(s"/$resourceDir/$name.txt")
  assertNotNull(s"Test resource not found: /$resourceDir/$name.txt", resource)
  val source = root.resolve("sources").resolve(name)
  Files.createDirectories(source.getParent)
  Files.copy(Paths.get(resource.toURI), source)
  source

/** Compile `sources` with the current compiler into `output`, using `classpath` ahead of the JVM classpath. */
def compileStage(output: Path, classpath: Seq[Path], sources: Path*): Unit =
  Files.createDirectories(output)
  val compilerClasspath =
    (classpath.map(_.toString) :+ javaClasspath).mkString(java.io.File.pathSeparator)
  val reporter = new TestReporter
  val result = dotty.tools.dotc.Main.process(
    Array("-classpath", compilerClasspath, "-d", output.toString) ++ sources.map(_.toString),
    reporter
  )
  assertFalse(
    s"Compilation failed:\n${reporter.errors.result().map(_.msg.message).mkString("\n")}",
    result.hasErrors
  )

/** Recursively collect `.tasty` files under `dir`. */
def collectTastyFiles(dir: Path): Seq[File] =
  def collect(f: File): List[File] = Option(f.listFiles).toList.flatten.flatMap {
    case d if d.isDirectory => collect(d)
    case t if t.getName.endsWith(".tasty") => t :: Nil
    case _ => Nil
  }
  collect(dir.toFile)