package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts._
import Flags._
import SymUtils._
import Symbols._
import Types._
import Decorators._
import DenotTransformers._
import Names._
import StdNames._
import SymDenotations._
import ast.Trees._
import NameKinds.ImplMethName

/** Rewrite calls
 *
 *    super[M].f(args)
 *
 *  where M is a trait implemented by the current class to
 *
 *    M.f$(this, args)
 *
 *  where f$ is a static member of M.
 *
 *  Also generates said static members, as forwarders to the normal methods.
 */
class LinkScala2Impls extends MiniPhase with SymTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "linkScala2Impls"
  override def changesMembers: Boolean = true

  override def runsAfterGroupsOf: Set[String] = Set(Mixin.name)
    // Adds as a side effect static members to traits which can confuse Mixin,
    // that's why it is runsAfterGroupOf

  override def transformSym(sym: SymDenotation)(using Context): SymDenotation =
    if sym.is(Trait) && !ctx.settings.scalajs.value then
      val mixin = sym.asClass.classSymbol
      val ops = new MixinOps(mixin, thisPhase)
      import ops._

      def newImpl(meth: TermSymbol): Symbol =
        def staticInfo(tp: Type) = tp match
          case mt: MethodType =>
            MethodType(nme.SELF :: mt.paramNames, mixin.typeRef :: mt.paramInfos, mt.resType)
        val mold = meth.ensureNotPrivate
        meth.copy(
          owner = mixin,
          name = staticForwarderName(mold.name.asTermName),
          flags = Method | JavaStatic,
          info = staticInfo(mold.info)
        )

      val classInfo = sym.asClass.classInfo
      val decls1 = classInfo.decls.cloneScope
      var modified: Boolean = false
      for (meth <- classInfo.decls)
        if needsStaticForwarder(meth) then
          decls1.enter(newImpl(meth.asTerm))
          modified = true
      if modified then
        sym.copySymDenotation(
          info = classInfo.derivedClassInfo(decls = decls1))
      else
        sym
    else
      sym
  end transformSym

  private def needsStaticForwarder(sym: Symbol)(using Context): Boolean =
    sym.is(Method, butNot = Deferred)

  private def staticForwarderName(name: TermName)(using Context): TermName =
    if name == nme.TRAIT_CONSTRUCTOR then name
    else ImplMethName(name)

  override def transformTemplate(impl: Template)(using Context): Tree =
    val sym = impl.symbol.owner.asClass
    if sym.is(Trait) && !ctx.settings.scalajs.value then
      val newConstr :: newBody = (impl.constr :: impl.body).flatMap {
        case stat: DefDef if needsStaticForwarder(stat.symbol) =>
          val meth = stat.symbol
          val staticForwarderDef = DefDef(implMethod(meth).asTerm, { paramss =>
            val params = paramss.head
            /* FIXME This is wrong. We are emitting a virtual call to `meth`,
             * but we must instead emit an `invokespecial` so that it is not
             * virtual. However, I don't know how to represent an invokevirtual
             * call on a non-`this` receiver. My best attempt is the following,
             * but that throws an exception when type-assigning the Super node:
             */
            //Apply(Super(params.head, sym.name.asTypeName, sym).select(meth), params.tail)
            Apply(params.head.select(meth), params.tail)
          })
          stat :: transformFollowing(staticForwarderDef) :: Nil
        case stat =>
          stat :: Nil
      }
      cpy.Template(impl)(constr = newConstr.asInstanceOf[DefDef], body = newBody)
    else
      impl

  override def transformApply(app: Apply)(using Context): Tree = {
    def currentClass = ctx.owner.enclosingClass.asClass
    app match {
      case Apply(sel @ Select(Super(_, _), _), args)
      if sel.symbol.owner.is(Trait) && currentClass.mixins.contains(sel.symbol.owner) && !ctx.settings.scalajs.value =>
        val impl = implMethod(sel.symbol)
        if (impl.exists) Apply(ref(impl), This(currentClass) :: args).withSpan(app.span)
        else app // could have been an abstract method in a trait linked to from a super constructor
      case _ =>
        app
    }
  }

  /** The implementation method of a super call or implementation class target */
  private def implMethod(meth: Symbol)(using Context): Symbol =
    val cls = meth.owner
    if meth.name == nme.TRAIT_CONSTRUCTOR then
      cls.info.decl(nme.TRAIT_CONSTRUCTOR).suchThat(_.is(JavaStatic)).symbol
    else
      cls.info.decl(staticForwarderName(meth.name.asTermName))
        .suchThat(c => FullParameterization.memberSignature(c.info) == meth.signature)
        .symbol
}
