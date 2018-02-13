package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, SymDenotations._, DenotTransformers._, Flags._
import util.Positions._
import SymUtils._
import StdNames._, NameOps._
import Decorators._

class MixinOps(cls: ClassSymbol, thisPhase: DenotTransformer)(implicit ctx: Context) {
  import ast.tpd._

  val superCls: Symbol = cls.superClass
  val mixins: List[ClassSymbol] = cls.mixins

  lazy val JUnit4Annotations: List[Symbol] = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").
    map(n => ctx.getClassIfDefined(s"org.junit.$n".toTypeName)).
    filter(_.exists)

  def implementation(member: TermSymbol): TermSymbol = {
    val res = member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred,
      info = cls.thisType.memberInfo(member)).enteredAfter(thisPhase).asTerm
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
      .withType(NamedType(sup.tpe, target))
    //sup.select(target)
  }

  /** Is `sym` a member of implementing class `cls`?
   *  The test is performed at phase `thisPhase`.
   */
  def isCurrent(sym: Symbol) =
    ctx.atPhase(thisPhase) { implicit ctx =>
      cls.info.member(sym.name).hasAltWith(_.symbol == sym)
      // this is a hot spot, where we spend several seconds while compiling stdlib
      // unfortunately it will discard and recompute all the member chaches,
      // both making itself slow and slowing down anything that runs after it
      // because resolveSuper uses hacks with explicit adding to scopes through .enter
      // this cannot be fixed by a smarter caching strategy. With current implementation
      // we HAVE to discard caches here for correctness
    }

  /** Does `method` need a forwarder to in  class `cls`
   *  Method needs a forwarder in those cases:
   *   - there's a class defining a method with same signature
   *   - there are multiple traits defining method with same signature
   */
  def needsForwarder(meth: Symbol): Boolean = {
    lazy val competingMethods = competingMethodsIterator(meth).toList

    def needsDisambiguation = competingMethods.exists(x=> !(x is Deferred)) // multiple implementations are available
    def hasNonInterfaceDefinition = competingMethods.exists(!_.owner.is(Trait)) // there is a definition originating from class
    meth.is(Method, butNot = PrivateOrAccessorOrDeferred) &&
    (meth.owner.is(Scala2x) || needsDisambiguation || hasNonInterfaceDefinition || needsJUnit4Fix(meth) ) &&
    isCurrent(meth)
  }

  private def needsJUnit4Fix(meth: Symbol): Boolean = {
    meth.annotations.nonEmpty && JUnit4Annotations.exists(annot => meth.hasAnnotation(annot))
  }

  /** Get `sym` of the method that needs a forwarder
    *  Method needs a forwarder in those cases:
    *   - there is a trait that defines a primitive version of implemented polymorphic method.
    *   - there is a trait that defines a polymorphic version of implemented primitive method.
    */
  def needsPrimitiveForwarderTo(meth: Symbol): Option[Symbol] = {
    def hasPrimitiveMissMatch(tp1: Type, tp2: Type): Boolean = (tp1, tp2) match {
      case (tp1: MethodicType, tp2: MethodicType) =>
        hasPrimitiveMissMatch(tp1.resultType, tp2.resultType) ||
        tp1.paramInfoss.flatten.zip(tp1.paramInfoss.flatten).exists(args => hasPrimitiveMissMatch(args._1, args._2))
      case _ =>
        def isPrimitiveOrValueClass(sym: Symbol): Boolean = sym.isPrimitiveValueClass || sym.isValueClass
        isPrimitiveOrValueClass(tp1.typeSymbol) ^ isPrimitiveOrValueClass(tp2.typeSymbol)
    }

    def needsPrimitiveForwarder(m: Symbol): Boolean =
      m.owner != cls && !m.is(Deferred) && hasPrimitiveMissMatch(meth.info, m.info)

    if (!meth.is(Method | Deferred, butNot = PrivateOrAccessor) || meth.overriddenSymbol(cls).exists || needsForwarder(meth)) None
    else competingMethodsIterator(meth).find(needsPrimitiveForwarder)
  }

  final val PrivateOrAccessor = Private | Accessor
  final val PrivateOrAccessorOrDeferred = Private | Accessor | Deferred

  def forwarder(target: Symbol) = (targs: List[Type]) => (vrefss: List[List[Tree]]) =>
    superRef(target).appliedToTypes(targs).appliedToArgss(vrefss)

  private def competingMethodsIterator(meth: Symbol): Iterator[Symbol] = {
    cls.baseClasses.iterator
      .filter(_ ne meth.owner)
      .map(meth.overriddenSymbol)
      .filter(_.exists)
  }
}
