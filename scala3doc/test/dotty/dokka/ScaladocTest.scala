package dotty.dokka

import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.{Test, Rule}
import org.junit.rules.{TemporaryFolder, ErrorCollector}
import java.io.File

abstract class ScaladocTest(val name: String):

  def afterRendering(op: DocContext ?=> Unit) =
    val ctx = Scala3doc.run(args)(using testContext)
    op(using ctx)


  def withModule(op: DocContext ?=> Module => Unit) =
    given DocContext = testDocContext
    op(ScalaModuleProvider.mkModule())

  private def getTempDir() : TemporaryFolder =
    val folder = new TemporaryFolder()
    folder.create()
    folder

  def args = Scala3doc.Args(
      name = "test",
      tastyFiles = tastyFiles(name),
      output = getTempDir().getRoot,
      projectVersion = Some("1.0")
    )

  @Test
  def runTest: Unit


  @Rule
  def collector = _collector
  private val _collector = new ErrorCollector();
  def reportError(msg: String) = collector.addError(new AssertionError(msg))


