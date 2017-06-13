package dotty.tools
package repl

import dotc.ast.tpd
import dotc.core.{ Phases, Decorators, Flags }
import dotc.core.Contexts.Context
import dotc.{ CompilationUnit, Compiler }
import backend.jvm.GenBCode
import io._
import results._

class ReplCompiler(ictx: Context) extends Compiler {
  /** A GenBCode phase that outputs to a virtual directory */
  private class REPLGenBCode extends GenBCode {
    override def phaseName = "replGenBCode"

    /** Directory to save class files to */
    private val virtualDirectory =
      if (ictx.settings.d.isDefault(ictx))
        new VirtualDirectory("(memory)", None)
      else
        new PlainDirectory(new Directory(new JFile(ictx.settings.d.value(ictx))))

    override def outputDir(implicit ctx: Context) = virtualDirectory
  }

  /** Phases without `FrontEnd`, and a modified `GenBCode` */
  override def phases = Phases.replace(
    classOf[GenBCode],
    _ => new REPLGenBCode :: Nil,
    super.phases.tail
  )

  def compile(tree: tpd.Tree)(implicit ctx: Context): Result[Unit] = ()
}
