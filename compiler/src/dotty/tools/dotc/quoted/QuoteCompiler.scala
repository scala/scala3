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
import dotty.tools.dotc.tastyreflect.ReflectionImpl
import dotty.tools.dotc.transform.ReifyQuotes
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.{Path, VirtualFile}

import scala.annotation.tailrec
import scala.concurrent.Promise
import scala.quoted.{Expr, QuoteContext, Type}

/** Compiler that takes the contents of a quoted expression `expr` and produces
 *  a class file with `class ' { def apply: Object = expr }`.
 */
class QuoteCompiler extends Compiler {

  /** Either `Left` with name of the classfile generated or `Right` with the value contained in the expression */
  private[this] var result: Either[String, Any] = null

  override protected def frontendPhases: List[List[Phase]] =
    List(List(new QuotedFrontend))

  override protected def picklerPhases: List[List[Phase]] =
    List(List(new ReifyQuotes))

  override def newRun(implicit ctx: Context): ExprRun = {
    reset()
    new ExprRun(this, ctx.addMode(Mode.ReadPositions))
  }

  def outputClassName: TypeName = "Generated$Code$From$Quoted".toTypeName

  /** Frontend that receives a scala.quoted.Expr or scala.quoted.Type as input */
  class QuotedFrontend extends Phase {
    import tpd._


    def phaseName: String = "quotedFrontend"

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
      units.flatMap {
        case exprUnit: ExprCompilationUnit =>
          val pos = Span(0)
          val assocFile = new VirtualFile("<quote>")

          // Places the contents of expr in a compilable tree for a class with the following format.
          // `package __root__ { class ' { def apply: Any = <expr> } }`
          val cls = ctx.newCompleteClassSymbol(defn.RootClass, outputClassName, EmptyFlags,
            defn.ObjectType :: Nil, newScope, coord = pos, assocFile = assocFile).entered.asClass
          cls.enter(ctx.newDefaultConstructor(cls), EmptyScope)
          val meth = ctx.newSymbol(cls, nme.apply, Method, ExprType(defn.AnyType), coord = pos).entered

          val quoted = PickledQuotes.quotedExprToTree(checkValidRunExpr(exprUnit.exprBuilder.apply(new QuoteContext(ReflectionImpl(ctx)))))(ctx.withOwner(meth))

          getLiteral(quoted) match {
            case Some(value) =>
              result = Right(value)
              None // Stop copilation here we already have the result
            case None =>
              val run = DefDef(meth, quoted)
              val classTree = ClassDef(cls, DefDef(cls.primaryConstructor.asTerm), run :: Nil)
              val tree = PackageDef(ref(defn.RootPackage).asInstanceOf[Ident], classTree :: Nil).withSpan(pos)
              val source = SourceFile.virtual("<quoted.Expr>", "")
              result = Left(outputClassName.toString)
              Some(CompilationUnit(source, tree, forceTrees = true))
          }
      }
    }

    private def checkValidRunExpr(expr: Expr[_]): Expr[_] = expr match {
      case expr: scala.internal.quoted.TastyTreeExpr[Tree] @unchecked =>
        throw new Exception("Cannot call `Expr.run` on an `Expr` that comes from a macro argument.")
      case _ => expr
    }

    /** Get the literal value if this tree only contains a literal tree */
    @tailrec private def getLiteral(tree: Tree): Option[Any] = tree match {
      case Literal(lit) => Some(lit.value)
      case Block(Nil, expr) => getLiteral(expr)
      case Inlined(_, Nil, expr) => getLiteral(expr)
      case _ => None
    }

    def run(implicit ctx: Context): Unit = unsupported("run")
  }

  class ExprRun(comp: QuoteCompiler, ictx: Context) extends Run(comp, ictx) {
    /** Unpickle and optionally compile the expression.
     *  Returns either `Left` with name of the classfile generated or `Right` with the value contained in the expression.
     */
    def compileExpr(exprBuilder:  QuoteContext => Expr[_]): Either[String, Any] = {
      val units = new ExprCompilationUnit(exprBuilder) :: Nil
      compileUnits(units)
      result
    }
  }
}
