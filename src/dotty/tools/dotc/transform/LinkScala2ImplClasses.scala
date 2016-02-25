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
import collection.mutable

/** Rewrite calls
 *
 *    super[M].f(args)
 *
 *  where M is a Scala2 trait implemented by the current class to
 *
 *    M$class.f(this, args)
 *
 *  provided the implementation class M$class defines a corresponding function `f`.
 */
class LinkScala2ImplClasses extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "linkScala2ImplClasses"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Mixin])

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
    val implInfo = meth.owner.implClass.info
    if (meth.isConstructor)
      implInfo.decl(nme.TRAIT_CONSTRUCTOR).symbol
    else
      implInfo.decl(meth.name)
      .suchThat(c => FullParameterization.memberSignature(c.info) == meth.signature)
      .symbol
  }

  private val Scala2xTrait = allOf(Scala2x, Trait)
}
