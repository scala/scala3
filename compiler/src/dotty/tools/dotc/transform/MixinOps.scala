package dotty.tools.dotc
package transform

import core.*
import Decorators.*, Symbols.*, Types.*, Contexts.*, DenotTransformers.*, Flags.*
import NameKinds.*
import util.Spans.*
import util.chaining.*

import StdNames.*, NameOps.*
import typer.Nullables

class MixinOps(cls: ClassSymbol, thisPhase: DenotTransformer)(using Context) {
  import ast.tpd.*

  val superCls: Symbol = cls.superClass
  val mixins: List[ClassSymbol] = cls.mixins

  lazy val JUnit4Annotations: List[Symbol] = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").
    map(n => getClassIfDefined("org.junit." + n)).
    filter(_.exists)

  def mkForwarderSym(member: TermSymbol, extraFlags: FlagSet = EmptyFlags): TermSymbol =
    member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred &~ Module | Synthetic | extraFlags,
      info = cls.thisType.memberInfo(member)
    )
    .enteredAfter(thisPhase)
    .asTerm.tap: forwarder =>
      forwarder.addAnnotations(member.annotations.filter(_.symbol != defn.TailrecAnnot))
      val paramSymss = forwarder.paramSymss // compute once; different from rawParamss
      forwarder.setParamss(paramSymss)
      atPhaseBeforeTransforms:
        for (src, dst) <- member.paramSymss.flatten.filter(!_.isType).zip(paramSymss.flatten) do
          dst.addAnnotations(src.annotations)

  def superRef(target: Symbol, span: Span = cls.span): Tree = {
    val sup = if (target.isConstructor && !target.owner.is(Trait))
      Super(This(cls), tpnme.EMPTY)
    else
      Super(This(cls), target.owner.name.asTypeName, target.owner)
    //println(i"super ref $target on $sup")
    ast.untpd.Select(sup.withSpan(span), target.name)
      .withType(NamedType(sup.tpe, target))
    //sup.select(target)
  }

  /** Is `sym` a member of implementing class `cls`?
   *  The test is performed at phase `thisPhase`.
   */
  def isInImplementingClass(sym: Symbol): Boolean =
    atPhase(thisPhase) {
      cls.info.nonPrivateMember(sym.name).hasAltWith(_.symbol == sym)
    }

  /**
   * `meth` needs a forwarder in class `cls` if
   * - A non-trait base class defines matching method. Example:
   *   class C {def f: Int}; trait T extends C {def f = 1}; class D extends T
   *   Even if C.f is abstract, the forwarder in D is needed, otherwise the JVM would
   *   resolve `D.f` to `C.f`, see jvms-6.5.invokevirtual.
   *
   * - There exists another concrete, matching method in any of the base classes, and
   *   the `mixinClass` does not itself extend that base class. In this case the
   *   forwarder is needed to disambiguate. Example:
   *     trait T1 {def f = 1}; trait T2 extends T1 {override def f = 2}; class C extends T2
   *   In C we don't need a forwarder for f because T2 extends T1, so the JVM resolves
   *   C.f to T2.f non-ambiguously. See jvms-5.4.3.3, "maximally-specific method".
   *     trait U1 {def f = 1}; trait U2 {self:U1 => override def f = 2}; class D extends U2
   *   In D the forwarder is needed, the interfaces U1 and U2 are unrelated at the JVM level.
   */
  def needsMixinForwarder(mixin: ClassSymbol, meth: Symbol): Boolean =
    lazy val competingMethods =
      cls.baseClasses.iterator
        .filter(_ ne meth.owner)
        .map(base => meth.overriddenSymbol(base, cls))
        .filter(_.exists)
    lazy val mixinSuperTraits = mixin.baseClasses.filter(_.is(Trait))
    lazy val needsForwarder = competingMethods.exists(m => {
      !m.owner.is(Trait) ||
        (!m.is(Deferred) && !mixinSuperTraits.contains(m.owner))
    })

    // JUnit 4 won't recognize annotated default methods, so always generate a forwarder for them.
    def generateJUnitForwarder: Boolean =
      meth.annotations.nonEmpty && JUnit4Annotations.exists(meth.hasAnnotation) &&
        ctx.settings.mixinForwarderChoices.isAtLeastJunit

    // Similarly, Java serialization won't take into account a readResolve/writeReplace default method.
    def generateSerializationForwarder: Boolean =
       (meth.name == nme.readResolve || meth.name == nme.writeReplace) && meth.info.paramNamess.flatten.isEmpty

    !meth.isConstructor
    && meth.is(Method, butNot = PrivateOrAccessorOrDeferred)
    && (!meth.is(JavaDefined) || !meth.owner.is(Sealed) || meth.owner.children.contains(cls))
    && (
         ctx.settings.mixinForwarderChoices.isTruthy
      || meth.owner.is(Scala2x)
      || needsForwarder
      || generateJUnitForwarder
      || generateSerializationForwarder
    )
    && isInImplementingClass(meth)
    && !meth.name.is(InlineAccessorName)
  end needsMixinForwarder

  private val PrivateOrAccessor: FlagSet = Private | Accessor
  private val PrivateOrAccessorOrDeferred: FlagSet = Private | Accessor | Deferred

  def forwarderRhsFn(target: Symbol): List[List[Tree]] => Tree =
    prefss =>
      val (targs, vargss) = splitArgs(prefss)
      val tapp = superRef(target).appliedToTypeTrees(targs)
      val rhs = vargss match
        case Nil | List(Nil) =>
          // Overriding is somewhat loose about `()T` vs `=> T`, so just pick
          // whichever makes sense for `target`
          tapp.ensureApplied
        case _ =>
          tapp.appliedToArgss(vargss)
      if ctx.explicitNulls && target.is(JavaDefined) && !ctx.phase.erasedTypes then
        // We may forward to a super Java member in resolveSuper phase.
        // Since this is still before erasure, the type can be nullable
        // and causes error during checking. So we need to enable
        // unsafe-nulls to construct the rhs.
        Block(Nullables.importUnsafeNulls :: Nil, rhs)
      else rhs
}
