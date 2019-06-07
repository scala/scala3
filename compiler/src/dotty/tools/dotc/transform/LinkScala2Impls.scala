package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import ast.Trees._
import NameKinds.ImplMethName

/** Rewrite calls
 *
 *    super[M].f(args)
 *
 *  where M is a Scala 2.x trait implemented by the current class to
 *
 *    M.f$(this, args)
 *
 *  where f$ is a static member of M.
 */
class LinkScala2Impls extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "linkScala2Impls"
  override def changesMembers: Boolean = true

  override def runsAfterGroupsOf: Set[String] = Set(Mixin.name)
    // Adds as a side effect static members to traits which can confuse Mixin,
    // that's why it is runsAfterGroupOf

  private def addStaticForwarders(mixin: ClassSymbol)(implicit ctx: Context): Unit = {
    val ops = new MixinOps(mixin, thisPhase)
    import ops._

    def newImpl(meth: TermSymbol): Symbol = {
      def staticInfo(tp: Type) = tp match {
        case mt: MethodType =>
          MethodType(nme.SELF :: mt.paramNames, mixin.typeRef :: mt.paramInfos, mt.resType)
      }
      val mold =
        if (meth.isConstructor)
          meth.copySymDenotation(
            name = nme.TRAIT_CONSTRUCTOR,
            info = MethodType(Nil, defn.UnitType))
        else meth.ensureNotPrivate
      meth.copy(
        owner = mixin,
        name = if (meth.isConstructor) mold.name.asTermName else ImplMethName(mold.name.asTermName),
        flags = Method | JavaStatic,
        info = staticInfo(mold.info)
      )
    }
    for (sym <- mixin.info.decls) {
      if (needsMixinForwarder(sym) || sym.isConstructor || sym.isGetter && sym.is(Lazy) || sym.is(Method, butNot = Deferred))
        newImpl(sym.asTerm).enteredAfter(thisPhase)
    }
    // The trait is now fully augmented so the flag isn't needed anymore.
    mixin.resetFlag(Scala2xPartiallyAugmented)
  }

  override def prepareForTemplate(impl: Template)(implicit ctx: Context): Context = {
    val cls = impl.symbol.owner.asClass
    for (mixin <- cls.mixins if (mixin.is(Scala2xPartiallyAugmented)))
      addStaticForwarders(mixin)
    ctx
  }

  override def transformApply(app: Apply)(implicit ctx: Context): Tree = {
    def currentClass = ctx.owner.enclosingClass.asClass
    app match {
      case Apply(sel @ Select(Super(_, _), _), args)
      if sel.symbol.owner.is(Scala2x) && currentClass.mixins.contains(sel.symbol.owner) =>
        val impl = implMethod(sel.symbol)
        if (impl.exists) Apply(ref(impl), This(currentClass) :: args).withSpan(app.span)
        else app // could have been an abstract method in a trait linked to from a super constructor
      case _ =>
        app
    }
  }

  /** The 2.12 implementation method of a super call or implementation class target */
  private def implMethod(meth: Symbol)(implicit ctx: Context): Symbol = {
    val implName = ImplMethName(meth.name.asTermName)
    val cls = meth.owner
    if (cls.is(Scala2xTrait))
      if (meth.isConstructor)
        cls.info.decl(nme.TRAIT_CONSTRUCTOR).symbol
      else
        cls.info.decl(implName)
          .suchThat(c => FullParameterization.memberSignature(c.info) == meth.signature)
          .symbol
    else throw new AssertionError(i"no impl method for $meth")
  }

  private val Scala2xTrait = allOf(Scala2x, Trait)
}
