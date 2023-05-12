package dotty.tools.pc.base

import dotty.tools.pc.util.BuildInfo
import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.junit.Assert.*
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.BlockJUnit4ClassRunner
import org.junit.runners.model.{FrameworkMethod, Statement}
import dotty.tools.pc.*
import dotty.tools.pc.utils._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.concurrent.{Executors, ScheduledExecutorService}
import scala.collection.immutable
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{ClasspathSearch, ExcludedPackagesHandler}
import scala.meta.pc.{PresentationCompiler, PresentationCompilerConfig}
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.internal.pc.PresentationCompilerConfigImpl

object TestResources:
  val scalaLibrary = BuildInfo.ideTestsDependencyClasspath.map(_.toPath).toSeq

  val index = TestingIndex(scalaLibrary.map(_.toString))
  val classpathSearch = ClasspathSearch.fromClasspath(scalaLibrary, ExcludedPackagesHandler.default)

class ReusableClassRunner(testClass: Class[BasePCSuite]) extends BlockJUnit4ClassRunner(testClass):
  private val instance: BasePCSuite = testClass.getDeclaredConstructor().newInstance()

  override def createTest(): AnyRef = instance
  override def withBefores(
      method: FrameworkMethod,
      target: Object,
      statement: Statement
  ): Statement =
    statement

  override def withAfters(
      method: FrameworkMethod,
      target: Object,
      statement: Statement
  ): Statement =
    new Statement():
      override def evaluate(): Unit =
        try
          statement.evaluate()
        finally
          if (isLastTestCase(method)) then instance.clean()

  private def isLastTestCase(method: FrameworkMethod): Boolean =
    val testMethods = getTestClass().getAnnotatedMethods(classOf[org.junit.Test])
    testMethods.asScala.last == method

@RunWith(classOf[ReusableClassRunner])
abstract class BasePCSuite extends PcAssertions:
  val tmp = Files.createTempDirectory("metals")
  val executorService: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor()
  val testingWorkspaceSearch = TestingWorkspaceSearch(TestResources.scalaLibrary.map(_.toString))

  lazy val presentationCompiler: PresentationCompiler =
    val myclasspath: Seq[Path] = TestResources.scalaLibrary
    val scalacOpts = scalacOptions(myclasspath)

    val search = new TestingSymbolSearch(
      testingWorkspaceSearch,
      if requiresJdkSources then Some(TestResources.index) else None,
      TestResources.classpathSearch
    )

    new ScalaPresentationCompiler()
      .withConfiguration(config)
      .withExecutorService(executorService)
      .withScheduledExecutorService(executorService)
      .withSearch(search)
      .newInstance("", myclasspath.asJava, scalacOpts.asJava)

  protected def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl().copy(snippetAutoIndent = false)

  private def inspectDialect(filename: String, code: String) =
    val file = tmp.resolve(filename)
    Files.write(file, code.getBytes(StandardCharsets.UTF_8))
    testingWorkspaceSearch.inputs.update(filename, code)

  def clean() =
    presentationCompiler.shutdown()
    recursivelyDelete(tmp)
    executorService.shutdown()

  private def recursivelyDelete(path: Path): Unit =
    Files
      .walk(path)
      .sorted(Comparator.reverseOrder)
      .map(_.toFile)
      .forEach(_.delete)

  protected def scalacOptions(classpath: Seq[Path]): Seq[String] = immutable.Seq.empty
  protected def requiresJdkSources: Boolean = false
  protected def requiresScalaLibrarySources: Boolean = false

  def params(code: String, filename: String = "test.scala"): (String, Int) =
    val code2 = code.replace("@@", "")
    val offset = code.indexOf("@@")
    if (offset < 0) {
      fail("missing @@")
    }
    inspectDialect(filename, code2)
    (code2, offset)

  def hoverParams(
      code: String,
      filename: String = "test.scala"
  ): (String, Int, Int) =
    val code2 = code.replace("@@", "").replace("%<%", "").replace("%>%", "")
    val positionOffset =
      code.replace("%<%", "").replace("%>%", "").indexOf("@@")
    val startOffset = code.replace("@@", "").indexOf("%<%")
    val endOffset = code.replace("@@", "").replace("%<%", "").indexOf("%>%")
    (positionOffset, startOffset, endOffset) match
      case (po, so, eo) if po < 0 && so < 0 && eo < 0 =>
        fail("missing @@ and (%<% and %>%)")
        (code2, so, eo)
      case (_, so, eo) if so >= 0 && eo >= 0 =>
        (code2, so, eo)
      case (po, _, _) =>
        (code2, po, po)

  def doc(e: JEither[String, MarkupContent]): String = {
    if (e == null) ""
    else if (e.isLeft) {
      " " + e.getLeft
    } else
      " " + e.getRight.getValue
  }.trim

  def sortLines(stableOrder: Boolean, string: String): String =
    val strippedString = string.linesIterator.toList.filter(_.nonEmpty)
    if (stableOrder) strippedString.mkString("\n")
    else strippedString.sorted.mkString("\n")

  extension (s: String)
    def triplequoted: String = s.replace("'''", "\"\"\"")

    def removeRanges: String =
      s.replace("<<", "")
        .replace(">>", "")
        .replaceAll("/\\*.+\\*/", "")

    def removePos: String = s.replace("@@", "")
