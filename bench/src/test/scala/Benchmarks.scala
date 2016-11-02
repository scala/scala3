package dotty.tools.benchmarks


import org.scalameter.Key.reports._
import org.scalameter.PerformanceTest.OnlineRegressionReport
import org.scalameter.api._
import org.scalameter.{Context, History, currentContext, persistence}
import org.scalameter.reporting.RegressionReporter.Tester
import dotty.tools.dotc.CompilerTest

import scala.io.Source

// decorator of persitor to expose info for debugging
class DecoratorPersistor(p: Persistor) extends SerializationPersistor {

  override def load(context: Context): History = {
    val resultdir = currentContext(resultDir)
    val scope = context.scope
    val curve = context.curve
    val fileName = s"$resultdir$sep$scope.$curve.dat"

    println(s"load file $fileName")

    p.load(context)
  }

  override def save(context: Context, h: History) = {
    val resultdir = currentContext(resultDir)
    val scope = context.scope
    val curve = context.curve
    val fileName = s"$resultdir$sep$scope.$curve.dat"

    println(s"save file $fileName")

    p.save(context, h)
  }
}

object BenchTests extends OnlineRegressionReport {
  val outputDir = "./out/"

  val compiler = new CompilerTest {
    override val defaultOutputDir: String = outputDir
  }

  implicit val defaultOptions = List("-d", outputDir)
  val scala2mode = List("-language:Scala2")

  val dottyDir  = "../compiler/src/dotty/"

  val stdlibFiles = Source.fromFile("../compiler/test/dotc/scala-collections.whitelist", "UTF8").getLines()
    .map(_.trim) // allow identation
    .filter(!_.startsWith("#")) // allow comment lines prefixed by #
    .map(_.takeWhile(_ != '#').trim) // allow comments in the end of line
    .filter(_.nonEmpty)
    .map("." + _)
    .toList

  def stdLib = compiler.compileList("compileStdLib", stdlibFiles, "-migration" :: scala2mode)

  def dotty = compiler.compileDir(dottyDir, ".",  List("-deep", "-strict"))

  // NOTE: use `val persistor = ...` would cause persistor ignore command line options for `resultDir`
  override def persistor = new DecoratorPersistor(super.persistor)

  // accept all the results, do not fail
  override def tester: Tester = new Tester.Accepter

  // store all results
  override def historian: RegressionReporter.Historian = RegressionReporter.Historian.Complete()

  override def executor: Executor = LocalExecutor(warmer, aggregator, measurer)


  def setup =
    performance of "dotty" in {
      measure.method("stdlib") in {
        using(Gen.unit("test")) curve "stdlib" in { r => stdLib }
      }

      measure.method("dotty-src") in {
        using(Gen.unit("test")) curve "dotty-src" in { r => dotty }
      }
    }

  /** workaround to fix problem in ScalaMeter
    *
    * NOTE: Otherwise, command line options would be ignored by HTMLReporter, as
    *       the HTMLReporter uses the context of tree node, which is created via
    *       ScalaMeter DSL before command line option `-CresultDir` takes effect
    *       in `PerformanceTest.main`.
    *
    *       Following code ensures that the test tree is set up after the `-CresultDir`
    *       option takes effect.
    **/
  override def executeTests(): Boolean = {
    setup
    super.executeTests()
  }

}
