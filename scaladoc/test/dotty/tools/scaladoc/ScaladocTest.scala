package dotty.tools.scaladoc

import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.{Test, Rule}
import org.junit.rules.{TemporaryFolder, ErrorCollector}
import java.io.File

abstract class ScaladocTest(val name: String):

  def afterRendering(op: DocContext ?=> Unit) =
    val ctx = Scaladoc.run(args)(using testContext)
    op(using ctx)

  def moduleDocContext = testDocContext(tastyFiles(name))

  def withModule(op: DocContext ?=> Module => Unit) =
    given DocContext = moduleDocContext
    op(ScalaModuleProvider.mkModule())

  protected def getTempDir() : TemporaryFolder =
    val folder = new TemporaryFolder()
    folder.create()
    folder

  def args = Scaladoc.Args(
      name = "test",
      tastyFiles = tastyFiles(name),
      output = getTempDir().getRoot,
      projectVersion = Some("1.0"),
      sourceLinks = List("github://lampepfl/dotty/master")
    )

  @Test
  def runTest: Unit


  @Rule
  def collector = _collector
  private val _collector = new ErrorCollector();
  def reportError(msg: String) = collector.addError(new AssertionError(msg))


