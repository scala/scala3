package dotty.tools.dotc
package quoted

import dotty.tools.backend.jvm.GenBCode
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Mode, Phases}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.transform.Pickler
import dotty.tools.io.VirtualDirectory

/** Compiler that takes the contents of a quoted expression `expr` and produces
 *  a class file with `class ' { def apply: Object = expr }`.
 */
class ExprCompiler(directory: VirtualDirectory) extends Compiler {

  /** A GenBCode phase that outputs to a virtual directory */
  private class ExprGenBCode extends GenBCode {
    override def phaseName = "genBCode"
    override def outputDir(implicit ctx: Context) = directory
  }

  override def phases: List[List[Phase]] = {
    val backendPhases = super.phases.dropWhile {
      case List(_: Pickler) => false
      case _ => true
    }.tail

    List(new ExprFrontend(putInClass = true)) ::
    Phases.replace(classOf[GenBCode], _ => new ExprGenBCode :: Nil, backendPhases)
  }

  override def newRun(implicit ctx: Context): ExprRun = {
    reset()
    new ExprRun(this, ctx.addMode(Mode.ReadPositions))
  }
}
