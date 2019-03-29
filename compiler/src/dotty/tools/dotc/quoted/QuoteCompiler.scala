package dotty.tools
package dotc
package quoted

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
import dotty.tools.dotc.transform.Staging
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{Path, VirtualFile}

import scala.quoted.{Expr, Type}

/** Compiler that takes the contents of a quoted expression `expr` and produces
 *  a class file with `class ' { def apply: Object = expr }`.
 */
class QuoteCompiler extends Compiler {

  override protected def frontendPhases: List[List[Phase]] =
    List(List(new QuotedFrontend(putInClass = true)))

  override protected def picklerPhases: List[List[Phase]] =
    List(List(new Staging))

  override def newRun(implicit ctx: Context): ExprRun = {
    reset()
    new ExprRun(this, ctx.addMode(Mode.ReadPositions))
  }

  def outputClassName: TypeName = "Generated$Code$From$Quoted".toTypeName

  /** Frontend that receives a scala.quoted.Expr or scala.quoted.Type as input */
  class QuotedFrontend(putInClass: Boolean) extends Phase {
    import tpd._

    def phaseName: String = "quotedFrontend"

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      units.map {
        case exprUnit: ExprCompilationUnit =>
          val tree =
            if (putInClass) inClass(exprUnit.expr)
            else PickledQuotes.quotedExprToTree(exprUnit.expr)
          val source = SourceFile.virtual("<quoted.Expr>", "")
          CompilationUnit(source, tree, forceTrees = true)
        case typeUnit: TypeCompilationUnit =>
          assert(!putInClass)
          val tree = PickledQuotes.quotedTypeToTree(typeUnit.tpe)
          val source = SourceFile.virtual("<quoted.Type>", "")
          CompilationUnit(source, tree, forceTrees = true)
      }
    }

    /** Places the contents of expr in a compilable tree for a class
      *  with the following format.
      *  `package __root__ { class ' { def apply: Any = <expr> } }`
      */
    private def inClass(expr: Expr[_])(implicit ctx: Context): Tree = {
      val pos = Span(0)
      val assocFile = new VirtualFile("<quote>")

      val cls = ctx.newCompleteClassSymbol(defn.RootClass, outputClassName, EmptyFlags,
        defn.ObjectType :: Nil, newScope, coord = pos, assocFile = assocFile).entered.asClass
      cls.enter(ctx.newDefaultConstructor(cls), EmptyScope)
      val meth = ctx.newSymbol(cls, nme.apply, Method, ExprType(defn.AnyType), coord = pos).entered

      val quoted = PickledQuotes.quotedExprToTree(expr)(ctx.withOwner(meth))

      val run = DefDef(meth, quoted)
      val classTree = ClassDef(cls, DefDef(cls.primaryConstructor.asTerm), run :: Nil)
      PackageDef(ref(defn.RootPackage).asInstanceOf[Ident], classTree :: Nil).withSpan(pos)
    }

    def run(implicit ctx: Context): Unit = unsupported("run")
  }

  class ExprRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
    def compileExpr(expr: Expr[_]): Unit = {
      val units = new ExprCompilationUnit(expr) :: Nil
      compileUnits(units)
    }
    def compileType(tpe: Type[_]): Unit = {
      val units = new TypeCompilationUnit(tpe) :: Nil
      compileUnits(units)
    }
  }
}
