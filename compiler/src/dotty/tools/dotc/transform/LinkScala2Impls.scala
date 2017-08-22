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
class LinkScala2Impls extends MiniPhase with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "linkScala2Impls"
  override def changesMembers = true
  val treeTransform = new Transform

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(classOf[Mixin])
    // Adds as a side effect static members to traits which can confuse Mixin,
    // that's why it is runsAfterGroupOf

  class Transform extends TreeTransform {
    def phase = thisTransform

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
        newImpl(sym.asTerm).enteredAfter(thisTransform)
    }

    override def prepareForTemplate(impl: Template)(implicit ctx: Context) = {
      val cls = impl.symbol.owner.asClass
      for (mixin <- cls.mixins)
        if (mixin.is(Scala_2_12_Trait, butNot = Scala_2_12_Augmented)) {
          augmentScala_2_12_Trait(mixin)
          mixin.setFlag(Scala_2_12_Augmented)
        }
      this
    }

    override def transformApply(app: Apply)(implicit ctx: Context, info: TransformerInfo) = {
      def currentClass = ctx.owner.enclosingClass.asClass
      app match {
        case Apply(sel @ Select(Super(_, _), _), args)
        if sel.symbol.owner.is(Scala2xTrait) && currentClass.mixins.contains(sel.symbol.owner) =>
          val impl = implMethod(sel.symbol)
          if (impl.exists) Apply(ref(impl), This(currentClass) :: args).withPos(app.pos)
          else app // could have been an abstract method in a trait linked to from a super constructor
        case _ =>
          app
      }
    }

    private def implMethod(meth: Symbol)(implicit ctx: Context): Symbol = {
      val (implInfo, implName) =
        if (meth.owner.is(Scala_2_12_Trait))
          (meth.owner.info, ImplMethName(meth.name.asTermName))
        else
          (meth.owner.implClass.info, meth.name)
      if (meth.isConstructor)
        implInfo.decl(nme.TRAIT_CONSTRUCTOR).symbol
      else
        implInfo.decl(implName)
          .suchThat(c => FullParameterization.memberSignature(c.info) == meth.signature)
          .symbol
    }
  }

  private val Scala2xTrait = allOf(Scala2x, Trait)
}
