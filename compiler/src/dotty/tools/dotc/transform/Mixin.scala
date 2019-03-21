package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameKinds._
import ast.Trees._
import collection.mutable

object Mixin {
  val name: String = "mixin"
}

/** This phase performs the following transformations:
 *
 *   1. (done in `traitDefs` and `transformSym`) Map every concrete trait getter
 *
 *       <mods> def x(): T = expr
 *
 *   to the pair of definitions:
 *
 *       <mods> def x(): T
 *       protected def initial$x(): T = { stats; expr }
 *
 *   where `stats` comprises all statements between either the start of the trait
 *   or the previous field definition which are not definitions (i.e. are executed for
 *   their side effects).
 *
 *   2. (done in `traitDefs`) Make every concrete trait setter
 *
 *      <mods> def x_=(y: T) = ()
 *
 *     deferred by mapping it to
 *
 *      <mods> def x_=(y: T)
 *
 *   3. For a non-trait class C:
 *
 *        For every trait M directly implemented by the class (see SymUtils.mixin), in
 *        reverse linearization order, add the following definitions to C:
 *
 *          3.1 (done in `traitInits`) For every parameter accessor `<mods> def x(): T` in M,
 *              in order of textual occurrence, add
 *
 *               <mods> def x() = e
 *
 *              where `e` is the constructor argument in C that corresponds to `x`. Issue
 *              an error if no such argument exists.
 *
 *          3.2 (done in `traitInits`) For every concrete trait getter `<mods> def x(): T` in M
 *              which is not a parameter accessor, in order of textual occurrence, produce the following:
 *
 *              3.2.1 If `x` is also a member of `C`, and is a lazy val,
 *
 *                <mods> lazy val x: T = super[M].x
 *
 *              3.2.2 If `x` is also a member of `C`, and M is a Dotty trait,
 *
 *                <mods> def x(): T = super[M].initial$x()
 *
 *              3.2.3 If `x` is also a member of `C`, and M is a Scala 2.x trait:
 *
 *                <mods> def x(): T = _
 *
 *              3.2.4 If `x` is not a member of `C`, and M is a Dotty trait:
 *
 *                super[M].initial$x()
 *
 *              3.2.5 If `x` is not a member of `C`, and M is a Scala2.x trait, nothing gets added.
 *
 *
 *          3.3 (done in `superCallOpt`) The call:
 *
 *                super[M].<init>
 *
 *          3.4 (done in `setters`) For every concrete setter `<mods> def x_=(y: T)` in M:
 *
 *                <mods> def x_=(y: T) = ()
 *
 *          3.5 (done in `mixinForwarders`) For every method
 *          `<mods> def f[Ts](ps1)...(psN): U` imn M` that needs to be disambiguated:
 *
 *                <mods> def f[Ts](ps1)...(psN): U = super[M].f[Ts](ps1)...(psN)
 *
 *          A method in M needs to be disambiguated if it is concrete, not overridden in C,
 *          and if it overrides another concrete method.
 *
 *   4. (done in `transformTemplate` and `transformSym`) Drop all parameters from trait
 *      constructors.
 *
 *   5. (done in `transformSym`) Drop ParamAccessor flag from all parameter accessors in traits.
 *
 *  Conceptually, this is the second half of the previous mixin phase. It needs to run
 *  after erasure because it copies references to possibly private inner classes and objects
 *  into enclosing classes where they are not visible. This can only be done if all references
 *  are symbolic.
 */
class Mixin extends MiniPhase with SymTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = Mixin.name

  override def relaxedTypingInGroup: Boolean = true
    // Because it changes number of parameters in trait initializers

  override def runsAfter: Set[String] = Set(Erasure.name)

  override def changesMembers: Boolean = true  // the phase adds implementions of mixin accessors

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.is(Accessor, butNot = Deferred) && sym.owner.is(Trait)) {
      val sym1 =
        if (sym is Lazy) sym
        else sym.copySymDenotation(initFlags = sym.flags &~ ParamAccessor | Deferred)
      sym1.ensureNotPrivate
    }
    else if (sym.isConstructor && sym.owner.is(Trait, butNot = Scala2x))
      sym.copySymDenotation(
        name = nme.TRAIT_CONSTRUCTOR,
        info = MethodType(Nil, sym.info.resultType))
    else
      sym

  private def initializer(sym: Symbol)(implicit ctx: Context): TermSymbol = {
    if (sym is Lazy) sym
    else {
      val initName = InitializerName(sym.name.asTermName)
      sym.owner.info.decl(initName).symbol
        .orElse(
          ctx.newSymbol(
            sym.owner,
            initName,
            Protected | Synthetic | Method,
            sym.info,
            coord = sym.coord).enteredAfter(thisPhase))
    }
  }.asTerm

  override def transformTemplate(impl: Template)(implicit ctx: Context): Template = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisPhase)
    import ops._

    def traitDefs(stats: List[Tree]): List[Tree] = {
      val initBuf = new mutable.ListBuffer[Tree]
      stats.flatMap({
        case stat: DefDef if stat.symbol.isGetter && !stat.rhs.isEmpty && !stat.symbol.is(Flags.Lazy) =>
          // make initializer that has all effects of previous getter,
          // replace getter rhs with empty tree.
          val vsym = stat.symbol
          val isym = initializer(vsym)
          val rhs = Block(
            initBuf.toList.map(_.changeOwnerAfter(impl.symbol, isym, thisPhase)),
            stat.rhs.changeOwnerAfter(vsym, isym, thisPhase).wildcardToDefault)
          initBuf.clear()
          cpy.DefDef(stat)(rhs = EmptyTree) :: DefDef(isym, rhs) :: Nil
        case stat: DefDef if stat.symbol.isSetter =>
          cpy.DefDef(stat)(rhs = EmptyTree) :: Nil
        case stat: DefTree =>
          stat :: Nil
        case stat =>
          initBuf += stat
          Nil
      }) ++ initBuf
    }

    /** Map constructor call to a pair of a supercall and a list of arguments
     *  to be used as initializers of trait parameters if the target of the call
     *  is a trait.
     */
    def transformConstructor(tree: Tree): (Tree, List[Tree]) = tree match {
      case Block(stats, expr) =>
        val (scall, inits) = transformConstructor(expr)
        (cpy.Block(tree)(stats, scall), inits)
      case _ =>
        val Apply(sel @ Select(New(_), nme.CONSTRUCTOR), args) = tree
        val (callArgs, initArgs) = if (tree.symbol.owner.is(Trait)) (Nil, args) else (args, Nil)
        (superRef(tree.symbol, tree.span).appliedToArgs(callArgs), initArgs)
    }

    val superCallsAndArgs = (
      for (p <- impl.parents; constr = stripBlock(p).symbol if constr.isConstructor)
      yield constr.owner -> transformConstructor(p)
    ).toMap
    val superCalls = superCallsAndArgs.mapValues(_._1)
    val initArgs = superCallsAndArgs.mapValues(_._2)

    def superCallOpt(baseCls: Symbol): List[Tree] = superCalls.get(baseCls) match {
      case Some(call) =>
        if (defn.NotRuntimeClasses.contains(baseCls) || baseCls.is(NoInitsTrait)) Nil
        else call :: Nil
      case None =>
        if (baseCls.is(NoInitsTrait) || defn.NoInitClasses.contains(baseCls) || defn.isFunctionClass(baseCls)) Nil
        else {
          //println(i"synth super call ${baseCls.primaryConstructor}: ${baseCls.primaryConstructor.info}")
          transformFollowingDeep(superRef(baseCls.primaryConstructor).appliedToNone) :: Nil
        }
    }

    def was(sym: Symbol, flags: FlagSet) =
      ctx.atPhase(thisPhase) { implicit ctx => sym is flags }

    def traitInits(mixin: ClassSymbol): List[Tree] = {
      var argNum = 0
      def nextArgument() = initArgs.get(mixin) match {
        case Some(arguments) =>
          val result = arguments(argNum)
          argNum += 1
          result
        case None =>
          assert(
              impl.parents.forall(_.tpe.typeSymbol != mixin),
              i"missing parameters for $mixin from $impl should have been caught in typer")
          ctx.error(
              em"""parameterized $mixin is indirectly implemented,
                  |needs to be implemented directly so that arguments can be passed""",
              cls.sourcePos)
          EmptyTree
      }

      for (getter <- mixin.info.decls.toList if getter.isGetter && !was(getter, Deferred)) yield {
        val isScala2x = mixin.is(Scala2x)
        def default = Underscore(getter.info.resultType)
        def initial = transformFollowing(superRef(initializer(getter)).appliedToNone)

        if (isCurrent(getter) || getter.name.is(ExpandedName)) {
          val rhs =
            if (was(getter, ParamAccessor))
              nextArgument()
            else if (isScala2x) {
              if (getter.is(Lazy, butNot = Module))
                initial
              else if (getter.is(Module))
                New(getter.info.resultType, List(This(cls)))
              else
                Underscore(getter.info.resultType)
            }
            else
              initial
          // transformFollowing call is needed to make memoize & lazy vals run
          transformFollowing(DefDef(mkForwarderSym(getter.asTerm), rhs))
        }
        else if (isScala2x || was(getter, ParamAccessor | Lazy)) EmptyTree
        else initial
      }
    }

    def setters(mixin: ClassSymbol): List[Tree] =
      for (setter <- mixin.info.decls.filter(setr => setr.isSetter && !was(setr, Deferred)))
        yield transformFollowing(DefDef(mkForwarderSym(setter.asTerm), unitLiteral.withSpan(cls.span)))

    def mixinForwarders(mixin: ClassSymbol): List[Tree] =
      for (meth <- mixin.info.decls.toList if needsMixinForwarder(meth))
      yield {
        util.Stats.record("mixin forwarders")
        transformFollowing(polyDefDef(mkForwarderSym(meth.asTerm, Bridge), forwarderRhsFn(meth)))
      }


    cpy.Template(impl)(
      constr =
        if (cls.is(Trait)) cpy.DefDef(impl.constr)(vparamss = Nil :: Nil)
        else impl.constr,
      parents = impl.parents.map(p => TypeTree(p.tpe).withSpan(p.span)),
      body =
        if (cls is Trait) traitDefs(impl.body)
        else {
          val mixInits = mixins.flatMap { mixin =>
            flatten(traitInits(mixin)) ::: superCallOpt(mixin) ::: setters(mixin) ::: mixinForwarders(mixin)
          }
          superCallOpt(superCls) ::: mixInits ::: impl.body
        })
  }
}
