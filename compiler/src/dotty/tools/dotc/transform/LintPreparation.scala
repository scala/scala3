
package dotty.tools.dotc.transform

/*
import scala.annotation.*

import dotty.tools.uncheckedNN
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd, untpd.ImportSelector
import dotty.tools.dotc.config.ScalaSettings
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.{em, i, toMessage}
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Types.{AnnotatedType, ClassInfo, ConstantType, NamedType, NoPrefix, NoType, TermRef, Type, TypeProxy, TypeTraverser}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{Name, TermName, termName}
import dotty.tools.dotc.core.NameOps.isReplWrapperName
import dotty.tools.dotc.core.NameKinds.{ContextFunctionParamName, WildcardParamName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, defn, isDeprecated}
import dotty.tools.dotc.report
import dotty.tools.dotc.reporting.{Message, UnusedSymbol as UnusedSymbolMessage}
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.{Property, SrcPos}
import dotty.tools.dotc.util.Spans.Span
import scala.util.chaining.given
*/

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Scopes, Scopes.MutableScope
import dotty.tools.dotc.core.Symbols.{Symbol, defn}
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.{Property}

//import scala.compiletime.uninitialized

/** A compiler phase that prepares contexts for linting.
 */
class LintPreparation extends MiniPhase:
  import LintPreparation.*

  override def phaseName = "lintPreparation"

  override def description = "prepare contexts for linting"

  override def isEnabled(using Context): Boolean = ctx.settings.WunusedHas.any

  override def isRunnable(using Context): Boolean = super.isRunnable && ctx.settings.WunusedHas.any && !ctx.isJava

  override def prepareForUnit(tree: Tree)(using Context): Context =
    ctx.fresh.setProperty(ctxKey, LintContexts())

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    ctx

  override def prepareForDefDef(tree: DefDef)(using Context): Context =
    assert(tree.symbol.owner eq ctx.owner, s"tree ${tree.symbol}, ctx ${ctx.owner}")
    pushLintContext(LintContext(tree.symbol))
    ctx
  override def transformDefDef(tree: DefDef)(using Context): tree.type =
    popLintContext() match
    case LintNamed(sym) => assert(sym eq tree.symbol)
    case x => throw MatchError(x)
    tree

  override def prepareForTemplate(tree: Template)(using Context): Context =
    assert(tree.symbol.owner eq ctx.owner, s"tree ${tree.symbol}, ctx ${ctx.owner}")
    pushLintContext(LintContext(ctx.owner))
    ctx
  override def transformTemplate(tree: Template)(using Context): Tree =
    popLintContext() match
    case LintNamed(sym) => assert(sym eq tree.symbol.owner)
    case x => throw MatchError(x)
    tree

  override def prepareForPackageDef(tree: PackageDef)(using Context): Context =
    if tree.symbol.isEmptyPackage then assert(ctx.owner eq defn.RootClass)
    else assert(tree.symbol eq ctx.owner, s"tree ${tree.symbol}, ctx ${ctx.owner}")
    pushLintContext(LintContext(tree.symbol))
    ctx
  override def transformPackageDef(tree: PackageDef)(using Context): Tree =
    popLintContext() match
    case LintNamed(sym) => assert(sym eq tree.symbol)
    case x => throw MatchError(x)
    tree

  override def prepareForStats(trees: List[Tree])(using Context): Context =
    val lctx = LintLocal()
    pushLintContext(lctx)
    trees.foreach:
      //case tree: MemberDef => lctx.scope.enter(tree.name, tree.symbol)
      case tree: MemberDef =>
        println(s"STATLOC ${tree.name} ${tree.symbol}")
        lctx.scope.enter(tree.name, tree.symbol)
      case _ =>
    ctx
  override def transformStats(trees: List[Tree])(using Context): List[Tree] =
    def popImports(): Unit =
      popLintContext() match
      case LintLocal() =>
      case LintImport(_) => popImports()
      case x => throw MatchError(x)
    popImports()
    trees

  override def prepareForOther(tree: Tree)(using Context): Context =
    tree match
    case imp: Import =>
      pushLintContext(LintContext(imp))
    case _ =>
    ctx

object LintPreparation:
  import collection.mutable, mutable.Stack

  private val ctxKey = Property.StickyKey[LintContexts]

  /** The context for linting.
   */
  inline def lintContext(using Context): LintContext =
    ctx.property(ctxKey) match
    case Some(ctxs) => ctxs.cur
    case None => throw new RuntimeException(s"no lint context")

  /** Iterate over all contexts for linting, inner to outer.
   */
  inline def lintContexts(using Context): Iterator[LintContext] =
    ctx.property(ctxKey) match
    case Some(ctxs) => ctxs.ctxs.iterator
    case None => Iterator.empty[LintContext]

  def pushLintContext(lintContext: LintContext)(using Context): Unit =
    ctx.property(ctxKey) match
    case Some(ctxs) => ctxs.push(lintContext)
    case None => throw new RuntimeException(s"no lint context")

  def popLintContext()(using Context): LintContext =
    ctx.property(ctxKey) match
    case Some(ctxs) => ctxs.pop()
    case None => throw new RuntimeException(s"no lint context")

  sealed abstract class LintContext:
    def introduces(name: Name): Boolean

  object LintContext:
    def apply(owner: Symbol)(using Context): LintContext = LintNamed(owner)
    def apply(tree: Import)(using Context): LintContext = LintImport(tree)
    def apply()(using Context): LintContext = LintLocal()

  case class LintNamed(owner: Symbol) extends LintContext:
    override def introduces(name: Name) = false

  case class LintImport(tree: Import) extends LintContext:
    override def introduces(name: Name) = false

  case class LintLocal() extends LintContext:
    override def introduces(name: Name) = false
    val scope: MutableScope = Scopes.newScope(0)

  class LintContexts:
    val ctxs = Stack.empty[LintContext]
    def cur: LintContext = ctxs.top
    def push(ctx: LintContext): ctx.type = { ctxs.push(ctx); ctx }
    def pop(): LintContext = ctxs.pop()

end LintPreparation
