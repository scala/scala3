package dotc.typer

import dotc.core._
import Names._
import Types._
import Flags._
import Symbols._
import Contexts._
import dotc.ast.Trees._
import dotc.util.{SourcePosition, Span}

/**
 * Cross-platform typer for the browser compiler.
 *
 * This is a simplified type checker that handles basic Scala constructs.
 */
class Typer {

  /** Type check a tree and return a typed tree */
  def typed(tree: Tree)(using ctx: Context): Tree = tree match {
    case lit: Literal => typedLiteral(lit)
    case id: Ident => typedIdent(id)
    case sel: Select => typedSelect(sel)
    case app: Apply => typedApply(app)
    case tapp: TypeApply => typedTypeApply(tapp)
    case block: Block => typedBlock(block)
    case ifExpr: If => typedIf(ifExpr)
    case whileExpr: WhileDo => typedWhile(whileExpr)
    case matchExpr: Match => typedMatch(matchExpr)
    case tryExpr: Try => typedTry(tryExpr)
    case fn: Function => typedFunction(fn)
    case valDef: ValDef => typedValDef(valDef)
    case defDef: DefDef => typedDefDef(defDef)
    case typeDef: TypeDef => typedTypeDef(typeDef)
    case template: Template => typedTemplate(template)
    case pkg: PackageDef => typedPackageDef(pkg)
    case imp: Import => typedImport(imp)
    case t: Typed => typedTyped(t)
    case n: New => typedNew(n)
    case ret: Return => typedReturn(ret)
    case thr: Throw => typedThrow(thr)
    case tuple: Tuple => typedTuple(tuple)
    case EmptyTree => EmptyTree
    case _ =>
      ctx.error(s"Cannot type check: ${tree.getClass.getSimpleName}", sourcePos(tree))
      tree
  }

  /** Type check multiple trees */
  def typedStats(stats: List[Tree])(using ctx: Context): List[Tree] =
    stats.map(typed)

  private def typedLiteral(lit: Literal)(using ctx: Context): Tree = {
    // Literal is already typed through its constant
    lit
  }

  private def typedIdent(id: Ident)(using ctx: Context): Tree = {
    val sym = ctx.lookupTerm(id.name.toTermName)
    if (!sym.exists) {
      ctx.error(s"Not found: ${id.name}", sourcePos(id))
    }
    id
  }

  private def typedSelect(sel: Select)(using ctx: Context): Tree = {
    val qual = typed(sel.qualifier)
    sel
  }

  private def typedApply(app: Apply)(using ctx: Context): Tree = {
    val fun = typed(app.fun)
    val args = app.args.map(typed)
    Apply(fun, args).withSpan(app.span)
  }

  private def typedTypeApply(tapp: TypeApply)(using ctx: Context): Tree = {
    val fun = typed(tapp.fun)
    TypeApply(fun, tapp.args).withSpan(tapp.span)
  }

  private def typedBlock(block: Block)(using ctx: Context): Tree = {
    val newCtx = ctx.fresh.setScope(new Scope)
    val stats = typedStats(block.stats)(using newCtx)
    val expr = typed(block.expr)(using newCtx)
    Block(stats, expr).withSpan(block.span)
  }

  private def typedIf(ifExpr: If)(using ctx: Context): Tree = {
    val cond = typed(ifExpr.cond)
    val thenp = typed(ifExpr.thenp)
    val elsep = typed(ifExpr.elsep)
    If(cond, thenp, elsep).withSpan(ifExpr.span)
  }

  private def typedWhile(whileExpr: WhileDo)(using ctx: Context): Tree = {
    val cond = typed(whileExpr.cond)
    val body = typed(whileExpr.body)
    WhileDo(cond, body).withSpan(whileExpr.span)
  }

  private def typedMatch(matchExpr: Match)(using ctx: Context): Tree = {
    val selector = typed(matchExpr.selector)
    val cases = matchExpr.cases.map(typedCaseDef)
    Match(selector, cases).withSpan(matchExpr.span)
  }

  private def typedCaseDef(caseDef: CaseDef)(using ctx: Context): CaseDef = {
    val pat = typedPattern(caseDef.pat)
    val guard = typed(caseDef.guard)
    val body = typed(caseDef.body)
    CaseDef(pat, guard, body).withSpan(caseDef.span).asInstanceOf[CaseDef]
  }

  private def typedPattern(pat: Tree)(using ctx: Context): Tree = {
    // Pattern type checking is simplified
    pat match {
      case bind: Bind =>
        val sym = newTermSymbol(ctx.owner, bind.name.toTermName)
        ctx.enter(sym)
        bind
      case _ => pat
    }
  }

  private def typedTry(tryExpr: Try)(using ctx: Context): Tree = {
    val expr = typed(tryExpr.expr)
    val cases = tryExpr.cases.map(typedCaseDef)
    val finalizer = typed(tryExpr.finalizer)
    Try(expr, cases, finalizer).withSpan(tryExpr.span)
  }

  private def typedFunction(fn: Function)(using ctx: Context): Tree = {
    val newCtx = ctx.fresh.setScope(new Scope)
    val params = fn.args.map { param =>
      val vd = param.asInstanceOf[ValDef]
      val sym = newTermSymbol(ctx.owner, vd.name, Param)
      newCtx.enter(sym)
      vd
    }
    val body = typed(fn.body)(using newCtx)
    Function(params, body).withSpan(fn.span)
  }

  private def typedValDef(valDef: ValDef)(using ctx: Context): Tree = {
    val sym = newTermSymbol(ctx.owner, valDef.name,
      if (valDef.mods.is(Mutable)) Mutable else EmptyFlags)
    ctx.enter(sym)

    val tpt = typed(valDef.tpt)
    val rhs = typed(valDef.rhs)

    ValDef(valDef.name, tpt, rhs).withMods(valDef.mods).withSpan(valDef.span)
  }

  private def typedDefDef(defDef: DefDef)(using ctx: Context): Tree = {
    val sym = newTermSymbol(ctx.owner, defDef.name, Method)
    ctx.enter(sym)

    val newCtx = ctx.fresh.setOwner(sym).setScope(new Scope)

    val paramss = defDef.paramss.map {
      case TermParamClause(params) =>
        TermParamClause(params.map(p => typedValDef(p)(using newCtx).asInstanceOf[ValDef]))
      case TypeParamClause(params) =>
        TypeParamClause(params.map(p => typedTypeDef(p)(using newCtx).asInstanceOf[TypeDef]))
    }

    val tpt = typed(defDef.tpt)(using newCtx)
    val rhs = typed(defDef.rhs)(using newCtx)

    DefDef(defDef.name, paramss, tpt, rhs).withMods(defDef.mods).withSpan(defDef.span)
  }

  private def typedTypeDef(typeDef: TypeDef)(using ctx: Context): Tree = {
    val sym = newTypeSymbol(ctx.owner, typeDef.name)
    ctx.enter(sym)

    val rhs = typed(typeDef.rhs)
    TypeDef(typeDef.name, rhs).withMods(typeDef.mods).withSpan(typeDef.span)
  }

  private def typedTemplate(template: Template)(using ctx: Context): Tree = {
    val constr = typedDefDef(template.constr)(using ctx).asInstanceOf[DefDef]
    val parents = template.parents.map(typed)
    val self = if (template.self != null) typedValDef(template.self)(using ctx).asInstanceOf[ValDef] else null
    val body = typedStats(template.body)
    Template(constr, parents, self, body).withSpan(template.span)
  }

  private def typedPackageDef(pkg: PackageDef)(using ctx: Context): Tree = {
    val stats = typedStats(pkg.stats)
    PackageDef(pkg.pid, stats).withSpan(pkg.span)
  }

  private def typedImport(imp: Import)(using ctx: Context): Tree = {
    // Import handling is simplified
    imp
  }

  private def typedTyped(t: Typed)(using ctx: Context): Tree = {
    val expr = typed(t.expr)
    Typed(expr, t.tpt).withSpan(t.span)
  }

  private def typedNew(n: New)(using ctx: Context): Tree = {
    val tpt = typed(n.tpt)
    New(tpt).withSpan(n.span)
  }

  private def typedReturn(ret: Return)(using ctx: Context): Tree = {
    val expr = typed(ret.expr)
    Return(expr, ret.from).withSpan(ret.span)
  }

  private def typedThrow(thr: Throw)(using ctx: Context): Tree = {
    val expr = typed(thr.expr)
    Throw(expr).withSpan(thr.span)
  }

  private def typedTuple(tuple: Tuple)(using ctx: Context): Tree = {
    val trees = tuple.trees.map(typed)
    Tuple(trees).withSpan(tuple.span)
  }

  private def sourcePos(tree: Tree)(using ctx: Context): SourcePosition =
    SourcePosition(ctx.source, tree.span)
}

/** Namer: enter definitions into scope */
class Namer {

  /** Enter all top-level definitions into the context */
  def enterAll(trees: List[Tree])(using ctx: Context): Unit = {
    trees.foreach(enter)
  }

  private def enter(tree: Tree)(using ctx: Context): Unit = tree match {
    case valDef: ValDef =>
      val sym = newTermSymbol(ctx.owner, valDef.name)
      ctx.enter(sym)

    case defDef: DefDef =>
      val sym = newTermSymbol(ctx.owner, defDef.name, Method)
      ctx.enter(sym)

    case typeDef: TypeDef =>
      val sym = newTypeSymbol(ctx.owner, typeDef.name)
      ctx.enter(sym)

    case classDef: ClassDef =>
      val sym = newClassSymbol(ctx.owner, classDef.name)
      ctx.enter(sym)

    case moduleDef: ModuleDef =>
      val sym = newTermSymbol(ctx.owner, moduleDef.name, Module)
      ctx.enter(sym)

    case pkg: PackageDef =>
      pkg.pid match {
        case id: Ident =>
          val pkgSym = newPackageSymbol(ctx.owner, id.name.toTermName)
          ctx.enter(pkgSym)
          val newCtx = ctx.fresh.setOwner(pkgSym).setScope(pkgSym.decls)
          enterAll(pkg.stats)(using newCtx)
        case _ =>
      }

    case _ => // Skip other trees
  }
}

