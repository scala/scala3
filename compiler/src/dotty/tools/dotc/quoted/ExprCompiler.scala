package dotty.tools.dotc
package quoted

import dotty.tools.backend.jvm.GenBCode
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Scopes.{EmptyScope, newScope}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.core.quoted.PickledQuotes
import dotty.tools.dotc.transform.ReifyQuotes
import dotty.tools.dotc.typer.FrontEnd
import dotty.tools.dotc.util.Positions.Position
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{AbstractFile, Path, PlainFile}

import scala.quoted.Expr

/** Compiler that takes the contents of a quoted expression `expr` and produces
 *  a class file with `class ' { def apply: Object = expr }`.
 */
class ExprCompiler(directory: AbstractFile) extends Compiler {
  import tpd._

  /** A GenBCode phase that outputs to a virtual directory */
  private class ExprGenBCode extends GenBCode {
    override def phaseName = "genBCode"
    override def outputDir(implicit ctx: Context) = directory
  }

  override protected def frontendPhases: List[List[Phase]] =
    List(List(new ExprFrontend(putInClass = true)))

  override protected def picklerPhases: List[List[Phase]] =
    List(List(new ReifyQuotes))

  override protected def backendPhases: List[List[Phase]] =
    List(List(new ExprGenBCode))

  override def newRun(implicit ctx: Context): ExprRun = {
    reset()
    new ExprRun(this, ctx.addMode(Mode.ReadPositions))
  }

  def outputClassName: TypeName = "Quoted".toTypeName

  /** Frontend that receives scala.quoted.Expr as input */
  class ExprFrontend(putInClass: Boolean) extends FrontEnd {
    import tpd._

    override def isTyper = false

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      units.map {
        case exprUnit: ExprCompilationUnit =>
          val tree =
            if (putInClass) inClass(exprUnit.expr)
            else PickledQuotes.quotedExprToTree(exprUnit.expr)
          val source = new SourceFile("", Seq())
          CompilationUnit.mkCompilationUnit(source, tree, forceTrees = true)
      }
    }

    /** Places the contents of expr in a compilable tree for a class
      *  with the following format.
      *  `package __root__ { class ' { def apply: Any = <expr> } }`
      */
    private def inClass(expr: Expr[_])(implicit ctx: Context): Tree = {
      val pos = Position(0)
      val assocFile = new PlainFile(Path("<quote>"))

      val cls = ctx.newCompleteClassSymbol(defn.RootClass, outputClassName, EmptyFlags,
        defn.ObjectType :: Nil, newScope, coord = pos.toCoord, assocFile = assocFile).entered.asClass
      cls.enter(ctx.newDefaultConstructor(cls), EmptyScope)
      val meth = ctx.newSymbol(cls, nme.apply, Method, ExprType(defn.AnyType), coord = pos.toCoord).entered

      val quoted = PickledQuotes.quotedExprToTree(expr)(ctx.withOwner(meth))

      val run = DefDef(meth, quoted)
      val classTree = ClassDef(cls, DefDef(cls.primaryConstructor.asTerm), run :: Nil)
      PackageDef(ref(defn.RootPackage).asInstanceOf[Ident], classTree :: Nil).withPos(pos)
    }
  }

  class ExprRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
    def compileExpr(expr: Expr[_]): Unit = {
      val units = new ExprCompilationUnit(expr) :: Nil
      compileUnits(units)
    }
  }

}
