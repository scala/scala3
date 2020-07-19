package dottyBench.tools.dotc
package core

import Contexts._, Symbols._, Types._, Flags._, Scopes._, Decorators._, NameOps._
import Denotations._
import SymDenotations.LazyType, Names.Name, StdNames.nme
import ast.untpd
import ast.Trees._
import typer.ImportInfo

/** Extension methods for contexts where we want to keep the ctx.<methodName> syntax */
object ContextOps:

  extension (ctx: Ctx):

    /** Enter symbol into current class, if current class is owner of current context,
    *  or into current scope, if not. Should always be called instead of scope.enter
    *  in order to make sure that updates to class members are reflected in
    *  finger prints.
    */
    def enter(sym: Symbol)(using CState): Symbol =
      given Ctx = ctx
      ctx.owner match
        case cls: ClassSymbol => cls.classDenot.enter(sym)
        case _ => scope.openForMutations.enter(sym)
      sym

    /** The denotation with the given `name` and all `required` flags in current context
     */
    def denotNamed(name: Name, required: FlagSet = EmptyFlags)(using CState): Denotation =
      given Ctx = ctx
      if (owner.isClass)
        if (outer.owner == owner) { // inner class scope; check whether we are referring to self
          if (scope.size == 1) {
            val elem = scope.lastEntry
            if (elem.name == name) return elem.sym.denot // return self
          }
          val pre = owner.thisType
          pre.findMember(name, pre, required, EmptyFlags)
        }
        else // we are in the outermost context belonging to a class; self is invisible here. See inClassContext.
          owner.findMember(name, owner.thisType, required, EmptyFlags)
      else
        scope.denotsNamed(name).filterWithFlags(required, EmptyFlags).toDenot(NoPrefix)

    /** Context where `sym` is defined, assuming we are in a nested context. */
    def defContext(sym: Symbol)(using CState): Ctx =
      given Ctx = ctx
      outersIterator
        .dropWhile(_.owner != sym)
        .dropWhile(_.owner == sym)
        .next()

    /** Either the current scope, or, if the current context owner is a class,
     *  the declarations of the current class.
     */
    def effectiveScope(using CState): Scope = ctx.owner match
      case cls: ClassSymbol =>
        given Ctx = ctx
        cls.unforcedDecls
      case _ => ctx.scope

    /** A fresh local context with given tree and owner.
     *  Owner might not exist (can happen for self valdefs), in which case
     *  no owner is set in result context
     */
    def localContext(tree: untpd.Tree, owner: Symbol)(using CState): FreshCtx =
      given Ctx = ctx
      val freshCtx = fresh.setTree(tree)
      if (owner.exists) freshCtx.setOwner(owner) else freshCtx

    /** A new context for the interior of a class */
    def inClassContext(selfInfo: TypeOrSymbol)(using CState): Ctx =
      given Ctx = ctx
      val localCtx = fresh.setNewScope
      selfInfo match {
        case sym: Symbol if sym.exists && sym.name != nme.WILDCARD => localCtx.scope.openForMutations.enter(sym)
        case _ =>
      }
      localCtx

    def packageContext(tree: untpd.PackageDef, pkg: Symbol)(using CState): Ctx =
      given Ctx = ctx
      if (pkg.is(Package)) fresh.setOwner(pkg.moduleClass).setTree(tree)
      else combinedContext

    /** The context for a supercall. This context is used for elaborating
     *  the parents of a class and their arguments.
     *  The context is computed from the current class context. It has
     *
     *  - as owner: The primary constructor of the class
     *  - as outer context: The context enclosing the class context
     *  - as scope: The parameter accessors in the class context
     *
     *  The reasons for this peculiar choice of attributes are as follows:
     *
     *  - The constructor must be the owner, because that's where any local methods or closures
     *    should go.
     *  - The context may not see any class members (inherited or defined), and should
     *    instead see definitions defined in the outer context which might be shadowed by
     *    such class members. That's why the outer context must be the outer context of the class.
     *  - At the same time the context should see the parameter accessors of the current class,
     *    that's why they get added to the local scope. An alternative would have been to have the
     *    context see the constructor parameters instead, but then we'd need a final substitution step
     *    from constructor parameters to class parameter accessors.
     */
    def superCallContext(using CState): Ctx =
      given Ctx = ctx
      val locals = newScopeWith(owner.typeParams ++ owner.asClass.paramAccessors: _*)
      superOrThisCallContext(owner.primaryConstructor, locals)

    /** The context for the arguments of a this(...) constructor call.
     *  The context is computed from the local auxiliary constructor context.
     *  It has
     *
     *   - as owner: The auxiliary constructor
     *   - as outer context: The context enclosing the enclosing class context
     *   - as scope: The parameters of the auxiliary constructor.
     */
    def thisCallArgContext(using CState): Ctx =
      given Ctx = ctx
      val constrCtx = outersIterator.dropWhile(_.outer.owner == owner).next()
      superOrThisCallContext(owner, constrCtx.scope)
        .setTyperState(typerState)
        .setGadt(gadt)
        .fresh
        .setScope(ctx.scope)

    /** The super- or this-call context with given owner and locals. */
    def superOrThisCallContext(owner: Symbol, locals: Scope)(using CState): FreshCtx =
      given Ctx = ctx
      var classCtx = ctx.outersIterator.dropWhile(!_.isClassDefContext).next()
      classCtx.outer.fresh.setOwner(owner)
        .setScope(locals)
        .setMode(classCtx.mode)

    /** The context of expression `expr` seen as a member of a statement sequence */
    def exprContext(stat: Tree[? >: Untyped], exprOwner: Symbol)(using CState): Ctx =
      given Ctx = ctx
      if (exprOwner == ctx.owner) combinedContext
      else if (untpd.isSuperConstrCall(stat) && ctx.owner.isClass) superCallContext
      else fresh.setOwner(exprOwner)

    /** A new context that summarizes an import statement */
    def importContext(imp: Import[?], sym: Symbol)(using CState): FreshCtx =
      given Ctx = ctx
      val impNameOpt = imp.expr match {
        case ref: RefTree[?] => Some(ref.name.asTermName)
        case _               => None
      }
      fresh.setImportInfo(ImportInfo(sym, imp.selectors, impNameOpt))

end ContextOps