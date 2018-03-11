package dotty.tools.dotc
package transform

import core._
import MegaPhase._
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
import ast.untpd
import ast.Trees._
import NameKinds.ImplMethName
import collection.mutable

/** Rewrite calls
 *
 *    super[M].f(args)
 *
 *  where M is a Scala 2.11 trait implemented by the current class to
 *
 *    M$class.f(this, args)
 *
 *  provided the implementation class M$class defines a corresponding function `f`.
 *  If M is a Scala 2.12 or newer trait, rewrite to
 *
 *    M.f(this, args)
 *
 *  where f is a static member of M.
 */
class LinkScala2Impls extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "linkScala2Impls"
  override def changesMembers = true

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(classOf[Mixin])
    // Adds as a side effect static members to traits which can confuse Mixin,
    // that's why it is runsAfterGroupOf

  /** Copy definitions from implementation class to trait itself */
  private def augmentScala_2_12_Trait(mixin: ClassSymbol)(implicit ctx: Context): Unit = {
    def info_2_12(sym: Symbol) = sym.info match {
      case mt @ MethodType(paramNames @ nme.SELF :: _) =>
        // 2.12 seems to always assume the enclsing mixin class as self type parameter,
        // whereas 2.11 used the self type of this class instead.
        val selfType :: otherParamTypes = mt.paramInfos
        MethodType(paramNames, mixin.typeRef :: otherParamTypes, mt.resType)
      case info => info
    }
    def newImpl(sym: TermSymbol): Symbol = sym.copy(
      owner = mixin,
      name = if (sym.isConstructor) sym.name else ImplMethName(sym.name),
      info = info_2_12(sym)
    )
    for (sym <- mixin.implClass.info.decls)
      newImpl(sym.asTerm).enteredAfter(thisPhase)
  }

  override def prepareForTemplate(impl: Template)(implicit ctx: Context) = {
    val cls = impl.symbol.owner.asClass
    for (mixin <- cls.mixins)
      if (mixin.is(Scala_2_12_Trait, butNot = Scala_2_12_Augmented)) {
        augmentScala_2_12_Trait(mixin)
        mixin.setFlag(Scala_2_12_Augmented)
      }
    ctx
  }

  override def transformApply(app: Apply)(implicit ctx: Context) = {
    def currentClass = ctx.owner.enclosingClass.asClass
    app match {
      case Apply(sel @ Select(Super(_, _), _), args)
      if sel.symbol.owner.isBoth(Scala2x, and = Trait) && currentClass.mixins.contains(sel.symbol.owner) =>
        val impl = implMethod(sel.symbol)
        if (impl.exists) Apply(ref(impl), This(currentClass) :: args).withPos(app.pos)
        else app // could have been an abstract method in a trait linked to from a super constructor
      case Apply(sel, args)
      if sel.symbol.maybeOwner.is(ImplClass) && sel.symbol.owner.traitOfImplClass.is(Scala_2_12_Trait) =>
        val impl = implMethod(sel.symbol)
        cpy.Apply(app)(ref(impl), args)
      case _ =>
        app
    }
  }

  /** The 2.12 implementation method of a super call or implementation class target */
  private def implMethod(meth: Symbol)(implicit ctx: Context): Symbol = {
    val implName = ImplMethName(meth.name.asTermName)
    val cls = meth.owner
    if (cls.is(ImplClass))
      cls.traitOfImplClass.info.decl(implName).atSignature(meth.signature).symbol
    else if (cls.is(Scala_2_12_Trait))
      if (meth.isConstructor)
        cls.info.decl(nme.TRAIT_CONSTRUCTOR).symbol
      else
        cls.info.decl(implName)
          .suchThat(c => FullParameterization.memberSignature(c.info) == meth.signature)
          .symbol
    else throw new AssertionError(i"no impl method for $meth")
  }
}
