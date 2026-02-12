package dotty.tools
package dotc
package interactive

import scala.language.unsafeNulls
import dotty.tools.dotc.transform.PruneSourcePath
import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.ast.tpd
import vulpix.TestConfiguration

import org.junit.Test
import org.junit.Assert._
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.printing.PlainPrinter
import dotty.tools.dotc.core.Decorators.show

/** Integration tests for sourcepath support in the compiler
 *
 * Tests that sources loaded from sourcepath can be properly pruned and referenced
 * from other compilation units.
 */
class SourcePathIntegrationTest extends DottyTest {

  override protected def defaultCompiler: Compiler = new InteractiveCompiler()
  override def initializeCtx(fc: FreshContext): Unit =
    super.initializeCtx(fc)
    // Enable the prune-sourcepath feature
    fc.setSetting(fc.settings.YpruneSourcepath, true)


  @Test def methodPruningWithExplicitType(): Unit = {

    val c = defaultCompiler
    val printer = new PlainPrinter(initialCtx)
    val run = c.newRun(using initialCtx)
    given Context = run.runContext.withoutColors

    val libraryCode = """
      package lib
      class Library {
        def publicMethod: String = "implementation"
        def anotherMethod(x: Int): Int = x * 2
      }
    """

    val librarySourceFile =
      SourceFile.virtual("library.scala", libraryCode)
    val libraryCompilationUnit = CompilationUnit(librarySourceFile, isFromSourcePath = true)(using run.runContext)
    val appCode = """
      package app
      import lib.Library
      object App {
        def main: Unit = {
          val lib = new Library
          def result = lib.publicMethod
          def compute = lib.anotherMethod(42)
        }
      }
    """

    val appSourceFile =
      SourceFile.virtual("app.scala", appCode)
    val appCompilationUnit = CompilationUnit(appSourceFile, isFromSourcePath = false)

    run.compileUnits(List(libraryCompilationUnit, appCompilationUnit))

    val findDefDef: (List[tpd.DefDef], tpd.Tree) => List[tpd.DefDef] =
      (acc , tree) =>  {
        tree match {
          case t: tpd.DefDef => t :: acc
          case _ => acc
        }
      }

    val d = new tpd.DeepFolder[List[tpd.DefDef]](findDefDef).foldOver(Nil, libraryCompilationUnit.tpdTree)
    def obtainedToString(defDefs: List[tpd.DefDef]): String =
      defDefs.collect{
        case df if !df.symbol.isConstructor => s"${df.symbol} tpt: ${df.tpt.symbol} rhs: ${df.rhs.showSummary()}"
      }.mkString("\n")
    val obtainedLib = obtainedToString(d)
    assertEquals(
      """|method anotherMethod tpt: class Int rhs: ???
         |method publicMethod tpt: type String rhs: ???""".stripMargin,
      obtainedLib
    )

    val appDefs = new tpd.DeepFolder[List[tpd.DefDef]](findDefDef).foldOver(Nil, appCompilationUnit.tpdTree)
    val obtainedApp = obtainedToString(appDefs)
    assertEquals(
      """|method compute tpt: class Int rhs: ....anotherMethod(42)
         |method result tpt: type String rhs: ...lib.publicMethod
         |method main tpt: class Unit rhs: {
         |  val lib: ... = ...
         |  def result: ... = ...
         |  def compute: ... = ...
         |  ()
         |}""".stripMargin,
      obtainedApp
    )

  }

}
