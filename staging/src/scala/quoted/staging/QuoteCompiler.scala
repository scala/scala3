package scala.quoted
package staging

import dotty.tools.unsupported
import dotty.tools.dotc._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Scopes.{EmptyScope, newScope}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.ExprType
import dotty.tools.dotc.quoted.PickledQuotes
import dotty.tools.dotc.transform.Splicer.checkEscapedVariables
import dotty.tools.dotc.transform.{Inlining, Staging, Splicing, PickleQuotes}
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.{SourceFile, NoSourcePosition}
import dotty.tools.io.{Path, VirtualFile}

import scala.quoted.runtime.impl._

import scala.annotation.tailrec
import scala.concurrent.Promise
import scala.quoted.{Expr, Quotes, Type}

/** Compiler that takes the contents of a quoted expression `expr` and produces
 *  a class file with `class ' { def apply: Object = expr }`.
 */
private class QuoteCompiler extends Compiler:

  /** Either `Left` with name of the classfile generated or `Right` with the value contained in the expression */
  private[this] var result: Either[String, Any] = null

  override protected def frontendPhases: List[List[Phase]] =
    List(List(new QuotedFrontend))

  override protected def picklerPhases: List[List[Phase]] =
    List(new Inlining) ::
    List(new Staging) ::
    List(new Splicing) ::
    List(new PickleQuotes) ::
    Nil

  override def newRun(implicit ctx: Context): ExprRun =
    reset()
    new ExprRun(this, ctx.addMode(Mode.ReadPositions))

  def outputClassName: TypeName = "Generated$Code$From$Quoted".toTypeName

  class RunScope extends Scope {
    override def toString: String = "scala.quoted.staging"
  }

  /** Frontend that receives a scala.quoted.Expr or scala.quoted.Type as input */
  class QuotedFrontend extends Phase:
    import tpd._

    def phaseName: String = "quotedFrontend"

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
      units.flatMap {
        case exprUnit: ExprCompilationUnit =>
          val ctx1 = ctx.fresh.setPhase(this.start).setCompilationUnit(exprUnit)
          implicit val unitCtx: Context = SpliceScope.setSpliceScope(new RunScope)(using ctx1)

          val pos = Span(0)
          val assocFile = new VirtualFile("<quote>")

          // Places the contents of expr in a compilable tree for a class with the following format.
          // `package __root__ { class ' { def apply: Any = <expr> } }`
          val cls = newCompleteClassSymbol(defn.RootClass, outputClassName, EmptyFlags,
            defn.ObjectType :: Nil, newScope, coord = pos, assocFile = assocFile).entered.asClass
          cls.enter(newDefaultConstructor(cls), EmptyScope)
          val meth = newSymbol(cls, nme.apply, Method, ExprType(defn.AnyType), coord = pos).entered

          val quoted =
            given Context = unitCtx.withOwner(meth)
            val qctx = QuotesImpl()
            val quoted = PickledQuotes.quotedExprToTree(exprUnit.exprBuilder.apply(qctx))
            checkEscapedVariables(quoted, meth)
          end quoted

          getLiteral(quoted) match
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

    /** Get the literal value if this tree only contains a literal tree */
    @tailrec private def getLiteral(tree: Tree): Option[Any] =
      tree match
        case Literal(lit) => Some(lit.value)
        case Block(Nil, expr) => getLiteral(expr)
        case Inlined(_, Nil, expr) => getLiteral(expr)
        case _ => None

    def run(implicit ctx: Context): Unit = unsupported("run")

  end QuotedFrontend

  class ExprRun(comp: QuoteCompiler, ictx: Context) extends Run(comp, ictx):
    /** Unpickle and optionally compile the expression.
     *  Returns either `Left` with name of the classfile generated or `Right` with the value contained in the expression.
     */
    def compileExpr(exprBuilder:  Quotes => Expr[_]): Either[String, Any] =
      val units = new ExprCompilationUnit(exprBuilder) :: Nil
      compileUnits(units)
      result

  end ExprRun

end QuoteCompiler
