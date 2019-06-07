package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, DenotTransformers._, Flags._
import util.Spans._
import SymUtils._
import StdNames._, NameOps._
import Decorators._

class MixinOps(cls: ClassSymbol, thisPhase: DenotTransformer)(implicit ctx: Context) {
  import ast.tpd._

  val superCls: Symbol = cls.superClass
  val mixins: List[ClassSymbol] = cls.mixins

  lazy val JUnit4Annotations: List[Symbol] = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").
    map(n => ctx.getClassIfDefined("org.junit." + n)).
    filter(_.exists)

  def mkForwarderSym(member: TermSymbol, extraFlags: FlagSet = EmptyFlags): TermSymbol = {
    val res = member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred | Synthetic | extraFlags,
      info = cls.thisType.memberInfo(member)).enteredAfter(thisPhase).asTerm
    res.addAnnotations(member.annotations.filter(_.symbol != defn.TailrecAnnot))
    res
  }

  def superRef(target: Symbol, span: Span = cls.span): Tree = {
    val sup = if (target.isConstructor && !target.owner.is(Trait))
      Super(This(cls), tpnme.EMPTY, true)
    else
      Super(This(cls), target.owner.name.asTypeName, false, target.owner)
    //println(i"super ref $target on $sup")
    ast.untpd.Select(sup.withSpan(span), target.name)
      .withType(NamedType(sup.tpe, target))
    //sup.select(target)
  }

  /** Is `sym` a member of implementing class `cls`?
   *  The test is performed at phase `thisPhase`.
   */
  def isCurrent(sym: Symbol): Boolean =
    ctx.atPhase(thisPhase) { implicit ctx =>
      cls.info.nonPrivateMember(sym.name).hasAltWith(_.symbol == sym)
    }

  /** Does `method` need a forwarder to in  class `cls`
   *  Method needs a forwarder in those cases:
   *   - there's a class defining a method with same signature
   *   - there are multiple traits defining method with same signature
   */
  def needsMixinForwarder(meth: Symbol): Boolean = {
    lazy val competingMethods = competingMethodsIterator(meth).toList

    def needsDisambiguation = competingMethods.exists(x=> !(x is Deferred)) // multiple implementations are available
    def hasNonInterfaceDefinition = competingMethods.exists(!_.owner.is(Trait)) // there is a definition originating from class
    !meth.isConstructor &&
    meth.is(Method, butNot = PrivateOrAccessorOrDeferred) &&
    (ctx.settings.mixinForwarderChoices.isTruthy || meth.owner.is(Scala2x) || needsDisambiguation || hasNonInterfaceDefinition || needsJUnit4Fix(meth)) &&
    isCurrent(meth)
  }

  private def needsJUnit4Fix(meth: Symbol): Boolean = {
    meth.annotations.nonEmpty && JUnit4Annotations.exists(annot => meth.hasAnnotation(annot)) &&
      ctx.settings.mixinForwarderChoices.isAtLeastJunit
  }

  final val PrivateOrAccessor: FlagSet = Private | Accessor
  final val PrivateOrAccessorOrDeferred: FlagSet = Private | Accessor | Deferred

  def forwarderRhsFn(target: Symbol): List[Type] => List[List[Tree]] => Tree =
    targs => vrefss => {
      val tapp = superRef(target).appliedToTypes(targs)
      vrefss match {
        case Nil | List(Nil) =>
          // Overriding is somewhat loose about `()T` vs `=> T`, so just pick
          // whichever makes sense for `target`
          tapp.ensureApplied
        case _ =>
          tapp.appliedToArgss(vrefss)
      }
    }

  private def competingMethodsIterator(meth: Symbol): Iterator[Symbol] = {
    cls.baseClasses.iterator
      .filter(_ ne meth.owner)
      .map(base => meth.overriddenSymbol(base, cls))
      .filter(_.exists)
  }
}
