package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, DenotTransformers._, Flags._
import util.Spans._
import SymUtils._
import StdNames._, NameOps._
import typer.Nullables

class MixinOps(cls: ClassSymbol, thisPhase: DenotTransformer)(using Context) {
  import ast.tpd._

  val superCls: Symbol = cls.superClass
  val mixins: List[ClassSymbol] = cls.mixins

  lazy val JUnit4Annotations: List[Symbol] = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").
    map(n => getClassIfDefined("org.junit." + n)).
    filter(_.exists)

  def mkForwarderSym(member: TermSymbol, extraFlags: FlagSet = EmptyFlags): TermSymbol = {
    val res = member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred &~ Module | Synthetic | extraFlags,
      info = cls.thisType.memberInfo(member)).enteredAfter(thisPhase).asTerm
    res.addAnnotations(member.annotations.filter(_.symbol != defn.TailrecAnnot))
    res
  }

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

  /** Does `method` need a forwarder to in  class `cls`
   *  Method needs a forwarder in those cases:
   *   - there's a class defining a method with same signature
   *   - there are multiple traits defining method with same signature
   */
  def needsMixinForwarder(meth: Symbol): Boolean = {
    lazy val competingMethods = competingMethodsIterator(meth).toList

    def needsDisambiguation = competingMethods.exists(x=> !x.is(Deferred)) // multiple implementations are available
    def hasNonInterfaceDefinition = competingMethods.exists(!_.owner.is(Trait)) // there is a definition originating from class

    // JUnit 4 won't recognize annotated default methods, so always generate a forwarder for them.
    def generateJUnitForwarder: Boolean =
      meth.annotations.nonEmpty && JUnit4Annotations.exists(annot => meth.hasAnnotation(annot)) &&
        ctx.settings.mixinForwarderChoices.isAtLeastJunit

    // Similarly, Java serialization won't take into account a readResolve/writeReplace default method.
    def generateSerializationForwarder: Boolean =
       (meth.name == nme.readResolve || meth.name == nme.writeReplace) && meth.info.paramNamess.flatten.isEmpty

    !meth.isConstructor &&
    meth.is(Method, butNot = PrivateOrAccessorOrDeferred) &&
    (ctx.settings.mixinForwarderChoices.isTruthy || meth.owner.is(Scala2x) || needsDisambiguation || hasNonInterfaceDefinition ||
     generateJUnitForwarder || generateSerializationForwarder) &&
    isInImplementingClass(meth)
  }

  final val PrivateOrAccessor: FlagSet = Private | Accessor
  final val PrivateOrAccessorOrDeferred: FlagSet = Private | Accessor | Deferred

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

  private def competingMethodsIterator(meth: Symbol): Iterator[Symbol] =
    cls.baseClasses.iterator
      .filter(_ ne meth.owner)
      .map(base => meth.overriddenSymbol(base, cls))
      .filter(_.exists)
}
