package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import ast.Trees._
import util.Positions._
import collection.mutable

/** This phase performs the following transformations:
 *
 *  1. (done in `traitDefs`) Map every concrete trait field
 *
 *       <mods> val x: T = expr
 *
 *   to the pair of definitions:
 *
 *       <mods> val x: T
 *       protected def initial$x: T = { stats; expr }
 *
 *   where `stats` comprises all statements between either the start of the trait
 *   or the previous field definition which are not definitions (i.e. are executed for
 *   their side effects).
 *
 *   2. (done in `traitDefs`) Make every concrete trait setter
 *
 *      <mods> def x_=(y: T) = ()
 *
 *     deferred by maping it to
 *
 *      <mods> def x_=(y: T)
 *
 *   3. For a non-trait class C:
 *
 *        For every trait M directly implemented by the class (see SymUtils.mixin), in
 *        reverse linearization order, add the following definitions to C:
 *
 *          3.1 (done in `traitInits`) For every concrete trait field `<mods> val x: T` in M,
 *              in order of textual occurrence:
 *
 *                <mods> val x: T = super[M].initial$x
 *
 *          3.2 (done in `constrCall`) The call:
 *
 *                super[M].<init>
 *
 *          3.3 (done in `setters`) For every concrete setter `<mods> def x_=(y: T)` in M:
 *
 *                <mods> def x_=(y: T) = ()
 *
 *          3.4 (done in `superAccessors`) For every superAccessor
 *              `<mods> def super$f[Ts](ps1)...(psN): U` in M:
 *
 *                <mods> def super$f[Ts](ps1)...(psN): U = super[S].f[Ts](ps1)...(psN)
 *
 *              where `S` is the superclass of `M` in the linearization of `C`.
 *
 *          3.5 (done in `methodOverrides`) For every method
 *              `<mods> def f[Ts](ps1)...(psN): U` in M` that needs to be disambiguated:
 *
 *                <mods> def f[Ts](ps1)...(psN): U = super[M].f[Ts](ps1)...(psN)
 *
 *        A method in M needs to be disambiguated if it is concrete, not overridden in C,
 *        and if it overrides another concrete method.
 */
class Mixin extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "mixin"

  override def treeTransformPhase = thisTransform.next

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym is Trait) {
      lazy val newDecls = sym.decls.cloneScope
      var hasInit = false
      for (d <- sym.decls)
        if (d.isTerm && !d.is(MethodOrDeferred)) {
          hasInit = true
          val initializer = ctx.newSymbol(
            sym.symbol,
            InitializerName(d.asTerm.name),
            Protected | Synthetic | Method,
            ExprType(d.info),
            coord = d.coord)
          newDecls.enter(initializer)
        }
      if (hasInit) {
        val cinfo = sym.asClass.classInfo
        sym.copySymDenotation(info = cinfo.derivedClassInfo(decls = newDecls))
      }
      else sym
    }
    else if ((sym.isField || sym.isSetter) && sym.owner.is(Trait) && !sym.is(Deferred))
      sym.copySymDenotation(initFlags = sym.flags | Deferred)
    else
      sym

  private val MethodOrDeferred = Method | Deferred
  private val PrivateOrDeferred = Private | Deferred

  private def traitDefs(cls: ClassSymbol, stats: List[Tree])(implicit ctx: Context): List[Tree] = {
    val initBuf = new mutable.ListBuffer[Tree]
    stats flatMap {
      case stat: ValDef if !stat.rhs.isEmpty =>
        val vsym = stat.symbol
        val isym = vsym.initializer
        val rhs = Block(
          initBuf.toList,
          stat.rhs.changeOwner(vsym, isym).ensureConforms(isym.info.widen))
        initBuf.clear()
        List(
          cpy.ValDef(stat)(mods = stat.mods | Deferred, rhs = EmptyTree),
          DefDef(stat.symbol.initializer, rhs))
      case stat: DefDef if stat.symbol.isSetter =>
        List(cpy.DefDef(stat)(
          mods = stat.mods | Deferred,
          rhs = EmptyTree))
      case stat: DefTree =>
        List(stat)
      case stat =>
        initBuf += stat
        Nil
    }
  }

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   */
  private def rebindSuper(base: Symbol, acc: Symbol)(implicit ctx: Context): Symbol = {
    var bcs = base.info.baseClasses.dropWhile(acc.owner != _).tail
    var sym: Symbol = NoSymbol
    val SuperAccessorName(memberName) = acc.name
    ctx.debuglog("starting rebindsuper " + base + " " + memberName + ":" + acc.info +
            " " + acc.owner + " " + base.info.baseClasses + "/" + bcs)
    while (!bcs.isEmpty && sym == NoSymbol) {
      val other = bcs.head.info.nonPrivateDecl(memberName)
      if (ctx.settings.debug.value)
        ctx.debuglog("rebindsuper " + bcs.head + " " + other + " " + other.info +
          " " + other.symbol.is(Deferred))
      sym = other.matchingDenotation(base.thisType, acc.info).symbol
      bcs = bcs.tail
    }
    sym
  }

  private def superAccessors(cls: ClassSymbol, mixin: ClassSymbol)(implicit ctx: Context): List[Tree] =
    for (superAcc <- mixin.decls.filter(_ is SuperAccessor).toList)
    yield polyDefDef(implementation(cls, superAcc.asTerm), forwarder(cls, rebindSuper(cls, superAcc)))

  private def traitInits(cls: ClassSymbol, mixin: ClassSymbol)(implicit ctx: Context): List[Tree] =
    for (field <- mixin.decls.filter(fld => fld.isField && !wasDeferred(fld)).toList)
    yield ValDef(implementation(cls, field.asTerm), superRef(cls, field.initializer, cls.pos))

  private def setters(cls: ClassSymbol, mixin: ClassSymbol)(implicit ctx: Context): List[Tree] =
    for (setter <- mixin.decls.filter(setr => setr.isSetter && !wasDeferred(setr)).toList)
    yield DefDef(implementation(cls, setter.asTerm), unitLiteral.withPos(cls.pos))

  private def implementation(cls: ClassSymbol, member: TermSymbol)(implicit ctx: Context): TermSymbol =
    member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred).enteredAfter(thisTransform).asTerm

  private def methodOverrides(cls: ClassSymbol, mixin: ClassSymbol)(implicit ctx: Context): List[Tree] = {
    def isOverridden(meth: Symbol) = meth.overridingSymbol(cls).is(Method, butNot = Deferred)
    def needsDisambiguation(meth: Symbol): Boolean =
      meth.is(Method, butNot = PrivateOrDeferred) &&
      !isOverridden(meth) &&
      !meth.allOverriddenSymbols.forall(_ is Deferred)
    for (meth <- mixin.decls.toList if needsDisambiguation(meth))
    yield polyDefDef(implementation(cls, meth.asTerm), forwarder(cls, meth))
  }

  private def wasDeferred(sym: Symbol)(implicit ctx: Context) =
    ctx.atPhase(thisTransform) { implicit ctx => sym is Deferred }

  private def superRef(cls: ClassSymbol, target: Symbol, pos: Position)(implicit ctx: Context) = {
    val inTrait = target.owner is Trait
    Super(
      qual = This(cls),
      mix = if (inTrait) target.owner.name.asTypeName else tpnme.EMPTY,
      inConstrCall = target.isConstructor && !target.owner.is(Trait),
      mixinClass = if (inTrait) target.owner else NoSymbol
    ).select(target)
  }

  private def forwarder(cls: ClassSymbol, target: Symbol)(implicit ctx: Context) =
    (targs: List[Type]) => (vrefss: List[List[Tree]]) =>
      superRef(cls, target, cls.pos).appliedToTypes(targs).appliedToArgss(vrefss)

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val cls = tree.symbol.owner.asClass
    val stats = tree.body
    val superCls = cls.classInfo.parents.head.symbol
    val mixins = cls.baseClasses.tail.takeWhile(_ ne superCls)

    // If parent is a constructor call, pull out the call into a separate
    // supercall constructor, which gets added to `superCalls`, and keep
    // only the type.
    val superCalls = new mutable.HashMap[Symbol, Tree]
    def normalizeParent(tree: Tree) = tree match {
      case superApp @ Apply(superSel @ Select(New(superType), nme.CONSTRUCTOR), superArgs) =>
        val constr = superSel.symbol
        superCalls(constr.owner) = superRef(cls, constr, superSel.pos).appliedToArgs(superArgs)
        superType
      case tree: TypeTree => tree
    }
    val parentTypeTrees = tree.parents.map(normalizeParent)

    def supCalls(baseCls: Symbol): List[Tree] = superCalls.remove(baseCls) match {
      case Some(call) => call :: Nil
      case None =>
        if (baseCls is Interface) Nil
        else superRef(cls, baseCls.primaryConstructor, cls.pos).appliedToNone :: Nil
    }

    cpy.Template(tree)(
      parents = parentTypeTrees,
      body =
        if (cls is Trait) traitDefs(cls, stats)
        else {
          val mixInits = mixins.flatMap { mixin =>
            assert(mixin is Trait)
            traitInits(cls, mixin) :::
            supCalls(mixin) :::
            setters(cls, mixin) :::
            superAccessors(cls, mixin) :::
            methodOverrides(cls, mixin)
          }
          supCalls(superCls) ::: mixInits ::: stats
        })
  }
}