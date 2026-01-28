package dotty.tools.pc.base

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.concurrent.{Executors, ScheduledExecutorService}

import scala.collection.immutable
import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{ClasspathSearch, ExcludedPackagesHandler}
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.{PresentationCompiler, PresentationCompilerConfig}
import scala.meta.pc.CompletionItemPriority

import dotty.tools.pc.*
import dotty.tools.pc.ScalaPresentationCompiler
import dotty.tools.pc.completions.CompletionSource
import dotty.tools.pc.tests.buildinfo.BuildInfo
import dotty.tools.pc.utils.*

import org.eclipse.lsp4j.MarkupContent
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.junit.runner.RunWith

object TestResources:
  val classpath: Seq[Path] = BuildInfo.ideTestsDependencyClasspath.map(_.toPath).toSeq
  val classpathSearch =
    ClasspathSearch.fromClasspath(classpath, ExcludedPackagesHandler.default)

@RunWith(classOf[ReusableClassRunner])
abstract class BasePCSuite extends PcAssertions:
  val completionItemPriority: CompletionItemPriority = (_: String) => 0
  protected val isDebug = ManagementFactory.getRuntimeMXBean.getInputArguments.toString.contains("-agentlib:jdwp")

  val tmp: Path = Files.createTempDirectory("stable-pc-tests")
  val executorService: ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor()
  val testingWorkspaceSearch = TestingWorkspaceSearch(
    TestResources.classpath.map(_.toString)
  )

  lazy val presentationCompiler: PresentationCompiler =
    val myclasspath: Seq[Path] = TestResources.classpath
    val scalacOpts = scalacOptions(myclasspath)
    val search = new MockSymbolSearch(
      testingWorkspaceSearch,
      TestResources.classpathSearch,
      mockEntries
    )

    new ScalaPresentationCompiler()
      .withConfiguration(config)
      .withExecutorService(executorService)
      .withScheduledExecutorService(executorService)
      .withSearch(search)
      .withCompletionItemPriority(completionItemPriority)
      .newInstance("", myclasspath.asJava, scalacOpts.asJava)

  protected def config: PresentationCompilerConfigImpl =
    PresentationCompilerConfigImpl().copy(snippetAutoIndent = false, timeoutDelay = if isDebug then 3600 else 10)

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

  protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    immutable.Seq.empty
  protected def mockEntries: MockEntries = new MockEntries {}

  def params(code: String, filename: String = "A.scala"): (String, Int) =
    val code2 = code.replace("@@", "")
    val offset = code.indexOf("@@")
    if offset < 0 then
      fail("missing @@")
    inspectDialect(filename, code2)
    (code2, offset)

  def hoverParams(
      code: String,
      filename: String = "A.scala"
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
    if e == null then ""
    else if e.isLeft then
      " " + e.getLeft
    else
      " " + e.getRight.getValue
  }.trim

  def sortLines(
      stableOrder: Boolean,
      string: String,
      completionSources: List[CompletionSource] = Nil
  ): (String, List[CompletionSource]) =
    val strippedString = string.linesIterator.toList.filter(_.nonEmpty)
    if stableOrder then strippedString.mkString("\n") -> completionSources
    else
      val paddedSources = completionSources.padTo(strippedString.size, CompletionSource.Empty)
      val (sortedCompletions, sortedSources) = (strippedString zip paddedSources).sortBy(_._1).unzip
      sortedCompletions.mkString("\n") -> sortedSources

  extension (s: String)
    def triplequoted: String = s.replace("'''", "\"\"\"")

    def removeRanges: String =
      s.replace("<<", "")
        .replace(">>", "")
        .replaceAll("/\\*.+\\*/", "")

    def removePos: String = s.replace("@@", "")
