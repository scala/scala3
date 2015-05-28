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
import Phases._
import ast.Trees._
import collection.mutable

/** This phase performs the following transformations:
 *
 *  1. (done in `traitDefs`) Map every concrete trait getter
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
 *          3.1 (done in `traitInits`) For every concrete trait getter `<mods> def x(): T` in M,
 *              in order of textual occurrence:
 *
 *                <mods> def x(): T = super[M].initial$x()
 *
 *          3.2 (done in `superCallOpt`) The call:
 *
 *                super[M].<init>
 *
 *          3.3 (done in `setters`) For every concrete setter `<mods> def x_=(y: T)` in M:
 *
 *                <mods> def x_=(y: T) = ()
 *
 *  Conceptually, this is the second half of the previous mixin phase. It needs to run
 *  after erasure because it copies references to possibly private inner classes and objects
 *  into enclosing classes where they are not visible. This can only be done if all references
 *  are symbolic.
 */
class Mixin extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "mixin"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Erasure])

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.is(Accessor, butNot = Deferred) && sym.owner.is(Trait))
      sym.copySymDenotation(initFlags = sym.flags | Deferred).ensureNotPrivate
    else
      sym

  private def initializer(sym: Symbol)(implicit ctx: Context): TermSymbol = {
    val initName = InitializerName(sym.name.asTermName)
    sym.owner.info.decl(initName).symbol
      .orElse(
        ctx.newSymbol(
          sym.owner,
          initName,
          Protected | Synthetic | Method,
          sym.info,
          coord = sym.symbol.coord).enteredAfter(thisTransform))
       .asTerm
  }

  override def transformTemplate(impl: Template)(implicit ctx: Context, info: TransformerInfo) = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisTransform)
    import ops._

    def traitDefs(stats: List[Tree]): List[Tree] = {
      val initBuf = new mutable.ListBuffer[Tree]
      stats.flatMap({
        case stat: DefDef if stat.symbol.isGetter && !stat.rhs.isEmpty && !stat.symbol.is(Flags.Lazy)  =>
          // make initializer that has all effects of previous getter,
          // replace getter rhs with empty tree.
          val vsym = stat.symbol
          val isym = initializer(vsym)
          val rhs = Block(
            initBuf.toList.map(_.changeOwner(impl.symbol, isym)),
            stat.rhs.changeOwner(vsym, isym))
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

    def transformSuper(tree: Tree): Tree = {
      val Apply(sel @ Select(New(_), nme.CONSTRUCTOR), args) = tree
      superRef(tree.symbol, tree.pos).appliedToArgs(args)
    }

    val superCalls = (
      for (p <- impl.parents if p.symbol.isConstructor)
      yield p.symbol.owner -> transformSuper(p)
    ).toMap

    def superCallOpt(baseCls: Symbol): List[Tree] = superCalls.get(baseCls) match {
      case Some(call) =>
        if (defn.PhantomClasses.contains(baseCls)) Nil else call :: Nil
      case None =>
        if (baseCls.is(NoInitsTrait) || defn.PhantomClasses.contains(baseCls)) Nil
        else {
          //println(i"synth super call ${baseCls.primaryConstructor}: ${baseCls.primaryConstructor.info}")
          superRef(baseCls.primaryConstructor).appliedToNone :: Nil
/*          constr.tpe.widen match {
            case tpe: PolyType =>
              val targs = cls.thisType.baseTypeWithArgs(baseCls).argTypes
              constr = constr.appliedToTypes(targs)
            case _ =>
          }
          constr.ensureApplied :: Nil
*/
        }
    }

    def wasDeferred(sym: Symbol) =
      ctx.atPhase(thisTransform) { implicit ctx => sym is Deferred }

    def traitInits(mixin: ClassSymbol): List[Tree] =
      for (getter <- mixin.info.decls.filter(getr => getr.isGetter && !wasDeferred(getr)).toList)
        yield {
        // transformFollowing call is needed to make memoize & lazy vals run
        val rhs = transformFollowing(superRef(initializer(getter)).appliedToNone)
        // isCurrent: getter is a member of implementing class
        val isCurrent = getter.is(ExpandedName) || ctx.atPhase(thisTransform) { implicit ctx =>
          cls.info.member(getter.name).suchThat(_.isGetter).symbol == getter
        }
        if (isCurrent) transformFollowing(DefDef(implementation(getter.asTerm), rhs))
        else rhs
      }

    def setters(mixin: ClassSymbol): List[Tree] =
      for (setter <- mixin.info.decls.filter(setr => setr.isSetter && !wasDeferred(setr)).toList)
        yield DefDef(implementation(setter.asTerm), unitLiteral.withPos(cls.pos))

    cpy.Template(impl)(
      parents = impl.parents.map(p => TypeTree(p.tpe).withPos(p.pos)),
      body =
        if (cls is Trait) traitDefs(impl.body)
        else {
          val mixInits = mixins.flatMap { mixin =>
            flatten(traitInits(mixin)) ::: superCallOpt(mixin) ::: setters(mixin)
          }
          superCallOpt(superCls) ::: mixInits ::: impl.body
        })
  }
}
