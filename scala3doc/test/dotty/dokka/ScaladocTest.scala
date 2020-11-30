package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.{DModule, WithChildren}
import org.jetbrains.dokka.pages.RootPageNode
import org.jetbrains.dokka.testApi.testRunner.{DokkaTestGenerator, TestMethods}
import org.jetbrains.dokka.testApi.logger.TestLogger
import org.jetbrains.dokka.utilities.DokkaConsoleLogger
import org.jetbrains.dokka.DokkaConfiguration
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.{Test, Rule}
import org.junit.rules.{TemporaryFolder, ErrorCollector}
import java.io.File
import dotty.tools.dotc.core.Contexts._

abstract class ScaladocTest(val name: String):
  def assertions: Seq[Assertion]

  private def getTempDir() : TemporaryFolder =
    val folder = new TemporaryFolder()
    folder.create()
    folder

  private def args = Scala3doc.Args(
      name = "test",
      tastyFiles = tastyFiles(name),
      output = getTempDir().getRoot,
      projectVersion = Some("1.0")
    )

  @Rule
  def collector = _collector
  private val _collector = new ErrorCollector();
  def reportError(msg: String) = collector.addError(new AssertionError(msg))

  @Test
  def executeTest =
    DokkaTestGenerator(
      DottyDokkaConfig(args, testContext),
      TestLogger(new Scala3DocDokkaLogger(using testContext)),
      assertions.asTestMethods,
      Nil.asJava
    ).generate()

end ScaladocTest

type Validator = () => Unit

/**
 * Those assertions map 1-1 to their dokka counterparts. Some of them may be irrelevant in scala3doc.
 */
enum Assertion:
  case AfterPluginSetup(fn: DokkaContext => Unit)
  case DuringValidation(fn: Validator => Unit)
  case AfterDocumentablesCreation(fn: Seq[DModule] => Unit)
  case AfterPreMergeDocumentablesTransformation(fn: Seq[DModule] => Unit)
  case AfterDocumentablesMerge(fn: DModule => Unit)
  case AfterDocumentablesTransformation(fn: DModule => Unit)
  case AfterPagesGeneration(fn: RootPageNode => Unit)
  case AfterPagesTransformation(fn: RootPageNode => Unit)
  case AfterRendering(fn: (RootPageNode, DokkaContext) => Unit)

extension (s: Seq[Assertion]):
  def asTestMethods: TestMethods =
    import Assertion._
    TestMethods(
      (context => s.collect { case AfterPluginSetup(fn) => fn(context) }.kUnit),
      (validator => s.collect { case DuringValidation(fn) => fn(() => validator.invoke()) }.kUnit),
      (modules => s.collect { case AfterDocumentablesCreation(fn) => fn(modules.asScala.toSeq) }.kUnit),
      (modules => s.collect { case AfterPreMergeDocumentablesTransformation(fn) => fn(modules.asScala.toSeq) }.kUnit),
      (module => s.collect { case AfterDocumentablesMerge(fn) => fn(module)}.kUnit),
      (module => s.collect { case AfterDocumentablesTransformation(fn) => fn(module) }.kUnit),
      (root => s.collect { case AfterPagesGeneration(fn) => fn(root) }.kUnit),
      (root => s.collect { case AfterPagesTransformation(fn) => fn(root) }.kUnit),
      ((root, context) => s.collect { case AfterRendering(fn) => fn(root, context)}.kUnit)
    )

extension [T] (s: T):
  private def kUnit = kotlin.Unit.INSTANCE
