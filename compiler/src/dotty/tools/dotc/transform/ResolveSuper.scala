package dotty.tools.dotc
package transform

import core.*
import MegaPhase.*
import Contexts.*
import Flags.*

import Symbols.*
import Decorators.*
import DenotTransformers.*
import Names.*
import NameOps.*
import NameKinds.*
import NullOpsDecorator.*
import ResolveSuper.*
import reporting.IllegalSuperAccessor

/** This phase implements super accessors in classes that need them.
 *
 *  For every trait M directly implemented by the class (see SymUtils.mixin), in
 *  reverse linearization order, add the following definitions to C:
 *
 *  For every superAccessor `<mods> def super$f[Ts](ps1)...(psN): U` in M:
 *
 *       <mods> def super$f[Ts](ps1)...(psN): U = super[S].f[Ts](ps1)...(psN)
 *
 *  where `S` is the superclass of `M` in the linearization of `C`.
 *
 *  This is the first part of what was the mixin phase. It is complemented by
 *  Mixin, which runs after erasure.
 */
class ResolveSuper extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd.*

  override def phaseName: String = ResolveSuper.name

  override def description: String = ResolveSuper.description

  override def runsAfter: Set[String] = Set(ElimByName.name, // verified empirically, need to figure out what the reason is.
                               PruneErasedDefs.name) // Erased decls make `isCurrent` work incorrectly

  override def changesMembers: Boolean = true // the phase adds super accessors

  override def transformTemplate(impl: Template)(using Context): Template = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisPhase)
    import ops.*

    def superAccessors(mixin: ClassSymbol): List[Tree] =
      for superAcc <- mixin.info.decls.filter(_.isSuperAccessor)
      yield
        util.Stats.record("super accessors")
        val fwd = mkForwarderSym(superAcc.asTerm)
        DefDef(fwd, forwarderRhsFn(rebindSuper(cls, superAcc))
          .andThen(_.etaExpandCFT(using ctx.withOwner(fwd))))

    val overrides = mixins.flatMap(superAccessors)

    cpy.Template(impl)(body = overrides ::: impl.body)
  }

  override def transformDefDef(ddef: DefDef)(using Context): Tree = {
    val meth = ddef.symbol.asTerm
    if (meth.isSuperAccessor && !meth.is(Deferred)) {
      assert(ddef.rhs.isEmpty, ddef.symbol)
      val cls = meth.owner.asClass
      val ops = new MixinOps(cls, thisPhase)
      import ops.*
      DefDef(meth, forwarderRhsFn(rebindSuper(cls, meth)))
    }
    else ddef
  }
}

object ResolveSuper {
  val name: String = "resolveSuper"
  val description: String = "implement super accessors"

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param acc        The symbol statically referred to by the superaccessor in the trait
   */
  def rebindSuper(base: Symbol, acc: Symbol)(using Context): Symbol = {
    var bcs = base.info.baseClasses.dropWhile(acc.owner != _).tail
    var sym: Symbol = NoSymbol

    def decomposeSuperName(superName: Name): (Name, TypeName) =
      superName.unexpandedName match
        case SuperAccessorName(ExpandPrefixName(name, mixName)) =>
          (name, mixName.toTypeName)
        case SuperAccessorName(name) =>
          (name, EmptyTypeName)

    val (memberName, mix) = decomposeSuperName(acc.name.unexpandedName)
    val targetName =
      if acc.name == acc.targetName then memberName
      else decomposeSuperName(acc.targetName)._1

    report.debuglog(i"starting rebindsuper from $base of ${acc.showLocated}: ${acc.info} in $bcs, name = $memberName")

    while (bcs.nonEmpty && sym == NoSymbol) {
      val other = bcs.head.info.nonPrivateDecl(memberName)
        .filterWithPredicate(denot => mix.isEmpty || denot.symbol.owner.name == mix)
        .matchingDenotation(base.thisType, base.thisType.memberInfo(acc), targetName)
      report.debuglog(i"rebindsuper ${bcs.head} $other deferred = ${other.symbol.is(Deferred)}")
      if other.exists && !other.symbol.is(Deferred) then
        sym = other.symbol
        // Having a matching denotation is not enough: it should also be a subtype
        // of the superaccessor's type, see i5433.scala for an example where this matters
        val otherTp = other.asSeenFrom(base.thisType).info
        val accTp = acc.asSeenFrom(base.thisType).info

        if !otherTp.overrides(accTp, matchLoosely = true) then
          report.error(IllegalSuperAccessor(base, memberName, targetName, acc, accTp, other.symbol, otherTp), base.srcPos)
      bcs = bcs.tail
    }
    if sym.is(Accessor) then
      report.error(
        em"parent ${acc.owner} has a super call which binds to the value ${sym.showFullName}. Super calls can only target methods.", base)
    sym.orElse {
      val originalName = acc.name.asTermName.originalOfSuperAccessorName
      report.error(em"Member method ${originalName.debugString} of mixin ${acc.owner} is missing a concrete super implementation in $base.", base.srcPos)
      acc
    }
  }
}
