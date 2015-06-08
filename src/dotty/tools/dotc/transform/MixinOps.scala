package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, SymDenotations._, DenotTransformers._, Flags._
import util.Positions._
import SymUtils._
import StdNames._, NameOps._

class MixinOps(cls: ClassSymbol, thisTransform: DenotTransformer)(implicit ctx: Context) {
  import ast.tpd._

  val superCls: Symbol = cls.superClass
  val mixins: List[ClassSymbol] = cls.mixins

  def implementation(member: TermSymbol): TermSymbol = {
    val res = member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred,
      info = cls.thisType.memberInfo(member)).enteredAfter(thisTransform).asTerm
    res.addAnnotations(member.annotations)
    res
  }

  def superRef(target: Symbol, pos: Position = cls.pos): Tree = {
    val sup = if (target.isConstructor && !target.owner.is(Trait))
      Super(This(cls), tpnme.EMPTY, true)
    else
      Super(This(cls), target.owner.name.asTypeName, false, target.owner)
    //println(i"super ref $target on $sup")
    ast.untpd.Select(sup.withPos(pos), target.name)
      .withType(NamedType.withFixedSym(sup.tpe, target))
    //sup.select(target)
  }

  /** Is `sym` a member of implementing class `cls`? */
  def isCurrent(sym: Symbol) = cls.info.member(sym.name).hasAltWith(_.symbol == sym)

  def needsForwarder(meth: Symbol): Boolean = {
    def needsDisambiguation = !meth.allOverriddenSymbols.forall(_ is Deferred)
    meth.is(Method, butNot = PrivateOrAccessorOrDeferred) &&
    isCurrent(meth) &&
    (needsDisambiguation || meth.owner.is(Scala2x))
  }

  final val PrivateOrAccessorOrDeferred = Private | Accessor | Deferred

  def forwarder(target: Symbol) = (targs: List[Type]) => (vrefss: List[List[Tree]]) =>
    superRef(target).appliedToTypes(targs).appliedToArgss(vrefss)
}
