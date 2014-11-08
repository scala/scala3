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
import collection.mutable

// todo: interface
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
 *     deferred by maping it to
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
 */
class Mixin extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "mixin"

  override def treeTransformPhase = thisTransform.next

  override def transformSym(sym: SymDenotation)(implicit ctx: Context): SymDenotation =
    if (sym.is(Accessor, butNot = Deferred) && sym.owner.is(Trait))
      sym.copySymDenotation(initFlags = sym.flags | Deferred)
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
      stats flatMap {
        case stat: DefDef if stat.symbol.isGetter && !stat.rhs.isEmpty =>
          val vsym = stat.symbol
          val isym = initializer(vsym)
          val rhs = Block(
            initBuf.toList.map(_.changeOwner(impl.symbol, isym)),
            stat.rhs.changeOwner(vsym, isym))
          initBuf.clear()
          List(
            cpy.DefDef(stat)(mods = stat.mods | Deferred, rhs = EmptyTree),
            DefDef(isym, rhs))
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
        if (baseCls.is(Interface) || defn.PhantomClasses.contains(baseCls)) Nil
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
      for (getter <- mixin.decls.filter(getr => getr.isGetter && !wasDeferred(getr)).toList)
        yield {
        DefDef(implementation(getter.asTerm), superRef(initializer(getter)).appliedToNone)
      }

    def setters(mixin: ClassSymbol): List[Tree] =
      for (setter <- mixin.decls.filter(setr => setr.isSetter && !wasDeferred(setr)).toList)
        yield DefDef(implementation(setter.asTerm), unitLiteral.withPos(cls.pos))

    cpy.Template(impl)(
      parents = impl.parents.map(p => TypeTree(p.tpe).withPos(p.pos)),
      body =
        if (cls is Trait) traitDefs(impl.body)
        else {
          val mixInits = mixins.flatMap { mixin =>
            traitInits(mixin) ::: superCallOpt(mixin) ::: setters(mixin)
          }
          superCallOpt(superCls) ::: mixInits ::: impl.body
        })
  }
}
