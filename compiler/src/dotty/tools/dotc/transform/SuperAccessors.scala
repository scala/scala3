package dotty.tools
package dotc
package transform

import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.mutable
import ValueClasses.isMethodWithExtension
import core._
import Contexts._, Flags._, Symbols._, Names._, StdNames._, NameOps._, Trees._
import TypeUtils._, SymUtils._
import DenotTransformers.DenotTransformer
import Symbols._
import util.Spans._
import Decorators._
import NameKinds.{ SuperAccessorName, ExpandPrefixName }

/** This class adds super accessors for all super calls that either
 *  appear in a trait or have as a target a member of some outer class.
 *
 *  It also checks that:
 *
 *  (1) Symbols accessed from super are not abstract, or are overridden by
 *  an abstract override.
 *
 *  (2) If a symbol accessed from super is defined in a real class (not a trait),
 *  there are no abstract members which override this member in Java's rules
 *  (see SI-4989; such an access would lead to illegal bytecode)
 *
 *  (3) Super calls do not go to some synthetic members of Any (see isDisallowed)
 *
 *  (4) Super calls do not go to synthetic field accessors
 */
class SuperAccessors(thisPhase: DenotTransformer) {

  import tpd._

  /** Some parts of trees will get a new owner in subsequent phases.
    *  These are value class methods, which will become extension methods.
    *  (By-name arguments used to be included also, but these
    *  don't get a new class anymore, they are just wrapped in a new method).
    *
    *  These regions will have to be treated specially for the purpose
    *  of adding accessors. For instance, super calls from these regions
    *  always have to go through an accessor.
    *
    *  The `invalidEnclClass` field, if different from NoSymbol,
    *  contains the symbol that is not a valid owner.
    */
  private var invalidEnclClass: Symbol = NoSymbol

  def withInvalidCurrentClass[A](trans: => A)(using Context): A = {
    val saved = invalidEnclClass
    invalidEnclClass = ctx.owner.enclosingClass
    try trans
    finally invalidEnclClass = saved
  }

  private def validCurrentClass(using Context): Boolean =
    ctx.owner.enclosingClass != invalidEnclClass

  /** List buffers for new accessor definitions, indexed by class */
  private val accDefs = MutableSymbolMap[mutable.ListBuffer[Tree]]()

  /** A super accessor call corresponding to `sel` */
  private def superAccessorCall(sel: Select, mixName: Name = nme.EMPTY)(using Context) = {
    val Select(qual, name) = sel
    val sym = sel.symbol
    val clazz = qual.symbol.asClass

    def superAccessorName(original: Name) =
      val unexpanded = SuperAccessorName(
        if mixName.isEmpty then original.toTermName
        else ExpandPrefixName(original.toTermName, mixName.toTermName))
      if clazz.is(Trait) then unexpanded.expandedName(clazz) else unexpanded

    val superName = superAccessorName(name)
    val superInfo = sel.tpe.widenSingleton.ensureMethodic
    val accRange = sel.span.focus
    val superAcc = clazz.info.decl(superName)
      .suchThat(_.signature == superInfo.signature).symbol
      .orElse {
        report.debuglog(s"add super acc ${sym.showLocated} to $clazz")
        val maybeDeferred = if (clazz.is(Trait)) Deferred else EmptyFlags
        val acc = newSymbol(
            clazz, superName, Artifact | Method | maybeDeferred,
            superInfo, coord = accRange).enteredAfter(thisPhase)
        acc.deriveTargetNameAnnotation(sym, superAccessorName)
        // Diagnostic for SI-7091
        if (!accDefs.contains(clazz))
          report.error(
            s"Internal error: unable to store accessor definition in ${clazz}. clazz.hasPackageFlag=${clazz.is(Package)}. Accessor required for ${sel} (${sel.show})",
            sel.srcPos)
        else accDefs(clazz) += DefDef(acc, EmptyTree).withSpan(accRange)
        acc
    }

    This(clazz).select(superAcc).withSpan(sel.span)
  }

  /** Check selection `super.f` for conforming to rules. If necessary,
    *  replace by a super accessor call.
    */
  private def transformSuperSelect(sel: Select)(using Context): Tree = {
    val Select(sup @ Super(_, mix), name) = sel: @unchecked
    val sym   = sel.symbol
    assert(sup.symbol.exists, s"missing symbol in $sel: ${sup.tpe}")
    val clazz = sup.symbol
    val currentClass = ctx.owner.enclosingClass

    if (sym.isTerm && !sym.is(Method, butNot = Accessor) && !ctx.owner.isAllOf(ParamForwarder))
      // ParamForwaders as installed ParamForwarding.scala do use super calls to vals
      report.error(s"super may be not be used on ${sym.underlyingSymbol}", sel.srcPos)
    else if (isDisallowed(sym))
      report.error(s"super not allowed here: use this.${sel.name} instead", sel.srcPos)
    else if (sym.is(Deferred)) {
      val member = sym.overridingSymbol(clazz.asClass)
      if (!mix.name.isEmpty ||
          !member.exists ||
          !(member.is(AbsOverride) && member.isIncompleteIn(clazz)))
        report.error(
            i"${sym.showLocated} is accessed from super. It may not be abstract unless it is overridden by a member declared `abstract' and `override'",
            sel.srcPos)
      else report.log(i"ok super $sel ${sym.showLocated} $member $clazz ${member.isIncompleteIn(clazz)}")
    }
    else {
      val owner = sym.owner
      if (!owner.is(Trait))
        if (mix.name.isEmpty)
          // scala/bug#4989 Check if an intermediate class between `clazz` and `sym.owner` redeclares the method as abstract.
          for (intermediateClass <- clazz.info.baseClasses.tail.takeWhile(_ != sym.owner)) {
            val overriding = sym.overridingSymbol(intermediateClass)
            if (overriding.is(Deferred, butNot = AbsOverride) && !overriding.owner.is(Trait))
              report.error(
                s"${sym.showLocated} cannot be directly accessed from ${clazz} because ${overriding.owner} redeclares it as abstract",
                sel.srcPos)
          }
        else {
          // scala/scala-dev#143:
          //   a call `super[T].m` that resolves to `A.m` cannot be translated to correct bytecode if
          //   `A` is a class (not a trait / interface), but not the direct superclass. Invokespecial
          //   would select an overriding method in the direct superclass, rather than `A.m`.
          //   We allow this if there are statically no intervening overrides.
          def hasClassOverride(member: Symbol, subCls: ClassSymbol): Boolean =
            if (subCls == defn.ObjectClass || subCls == member.owner) false
            else if (member.overridingSymbol(subCls).exists) true
            else hasClassOverride(member, subCls.superClass.asClass)
          val superCls = clazz.asClass.superClass.asClass
          if (owner != superCls && hasClassOverride(sym, superCls))
            report.error(
              em"""Super call cannot be emitted: the selected $sym is declared in $owner, which is not the direct superclass of $clazz.
              |An unqualified super call (super.${sym.name}) would be allowed.""",
              sel.srcPos)
        }
    }

    val needAccessor = name.isTermName && (
      clazz != currentClass || !validCurrentClass || mix.name.isEmpty && clazz.is(Trait))

    if (needAccessor) atPhase(thisPhase.next)(superAccessorCall(sel, mix.name))
    else sel
  }

  /** Disallow some super.XX calls targeting Any methods which would
    *  otherwise lead to either a compiler crash or runtime failure.
    */
  private def isDisallowed(sym: Symbol)(using Context) =
    sym.isTypeTestOrCast ||
    (sym eq defn.Any_==) ||
    (sym eq defn.Any_!=) ||
    (sym eq defn.Any_##)

  /** Transform select node, adding super and protected accessors as needed */
  def transformSelect(tree: Tree, targs: List[Tree])(using Context): Tree = {
    val sel @ Select(qual, name) = tree: @unchecked
    val sym = sel.symbol

    /** If an accesses to protected member of a class comes from a trait,
      *  or would need a protected accessor placed in a trait, we cannot
      *  perform the access to the protected member directly since jvm access
      *  restrictions require the call site to be in an actual subclass and
      *  traits don't count as subclasses in this respect. In this case
      *  we generate a super accessor instead. See SI-2296.
      */
    def needsSuperAccessor =
      ProtectedAccessors.needsAccessorIfNotInSubclass(sym) &&
      AccessProxies.hostForAccessorOf(sym).is(Trait)
    qual match {
      case _: This if needsSuperAccessor =>
        /*
          * A trait which extends a class and accesses a protected member
          *  of that class cannot implement the necessary accessor method
          *  because jvm access restrictions require the call site to be in
          *  an actual subclass and traits don't count as subclasses in this
          *  respect. We generate a super accessor itself, which will be fixed
          *  by the implementing class.  See SI-2296.
          */
        superAccessorCall(sel)
      case Super(_, mix) =>
        transformSuperSelect(sel)
      case _ =>
        sel
    }
  }

  /** Wrap template to template transform `op` with needed initialization and finalization */
  def wrapTemplate(tree: Template)(op: Template => Template)(using Context): Template = {
    accDefs(currentClass) = new mutable.ListBuffer[Tree]
    val impl = op(tree)
    val accessors = accDefs.remove(currentClass).nn
    if (accessors.isEmpty) impl
    else {
      val (params, rest) = impl.body span {
        case td: TypeDef => !td.isClassDef
        case vd: ValOrDefDef => vd.symbol.flags.is(ParamAccessor)
        case _ => false
      }
      cpy.Template(impl)(body = params ++ accessors ++ rest)
    }
  }

  /** Wrap `DefDef` producing operation `op`, potentially setting `invalidClass` info */
  def wrapDefDef(ddef: DefDef)(op: => DefDef)(using Context): DefDef =
    if (isMethodWithExtension(ddef.symbol)) withInvalidCurrentClass(op) else op
}
