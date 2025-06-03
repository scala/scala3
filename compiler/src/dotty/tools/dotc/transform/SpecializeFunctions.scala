package dotty.tools.dotc
package transform

import ast.Trees.*, ast.tpd, core.*
import Contexts.*, Types.*, Decorators.*, Symbols.*, DenotTransformers.*
import SymDenotations.*, Scopes.*, StdNames.*, NameOps.*, Names.*, NameKinds.*
import MegaPhase.MiniPhase


/** Specializes classes that inherit from `FunctionN` where there exists a
 *  specialized form.
 */
class SpecializeFunctions extends MiniPhase {
  import ast.tpd.*

  override def phaseName: String = SpecializeFunctions.name

  override def description: String = SpecializeFunctions.description

  override def runsAfter = Set(ElimByName.name)

  override def isEnabled(using Context): Boolean =
    !ctx.settings.scalajs.value

  /** Create forwarders from the generic applys to the specialized ones.
   */
  override def transformDefDef(ddef: DefDef)(using Context) = {
    // Note on special case for inline `apply`s:
    //   `apply` and `apply$retainedBody` are specialized in this transformation.
    //   `apply$retainedBody` have the name kind `BodyRetainerName`, these contain
    //   the runtime implementation of an inline `apply` that implements (or overrides)
    //   the `FunctionN.apply` method. The inline method is not specialized, it will
    //   be replaced with the implementation of `apply$retainedBody`. The following code
    //      inline def apply(x: Int): Double = x.toDouble:Double
    //      private def apply$retainedBody(x: Int): Double = x.toDouble:Double
    //   in is transformed into
    //      inline def apply(x: Int): Double = x.toDouble:Double
    //      private def apply$retainedBody(x: Int): Double = this.apply$mcDI$sp(x)
    //      def apply$mcDI$sp(v: Int): Double = x.toDouble:Double
    //   after erasure it will become
    //      def apply(v: Int): Double = this.apply$mcDI$sp(v) // from apply$retainedBody
    //      def apply$mcDI$sp(v: Int): Double = v.toDouble():Double
    //      def apply(v1: Object): Object = Double.box(this.apply(Int.unbox(v1))) // erasure bridge

    if ddef.name.asTermName.exclude(BodyRetainerName) != nme.apply
       || ddef.termParamss.length != 1
       || ddef.termParamss.head.length > 2
       || !ctx.owner.isClass
    then
      return ddef

    val sym = ddef.symbol
    val cls = ctx.owner.asClass

    var specName: Name | Null = null

    def isSpecializable = {
      val paramTypes = ddef.termParamss.head.map(_.symbol.info)
      val retType = sym.info.finalResultType
      specName = nme.apply.specializedFunction(retType, paramTypes)
      defn.isSpecializableFunction(cls, paramTypes, retType)
    }

    if (sym.is(Flags.Deferred) || sym.is(Flags.Inline) || !isSpecializable) return ddef

    val specializedApply = newSymbol(
        cls,
        specName.nn,
        (sym.flags | Flags.Synthetic) &~ Flags.Private, // Private flag can be set if the name is a BodyRetainerName
        sym.info
      ).entered

    val specializedDecl =
      DefDef(specializedApply.asTerm, vparamss => {
        ddef.rhs
          .changeOwner(ddef.symbol, specializedApply)
          .subst(ddef.termParamss.head.map(_.symbol), vparamss.head.map(_.symbol))
      })

    // create a forwarding to the specialized apply
    val args = ddef.termParamss.head.map(vparam => ref(vparam.symbol))
    val rhs = This(cls).select(specializedApply).appliedToTermArgs(args)
    val ddef1 = cpy.DefDef(ddef)(rhs = rhs)
    Thicket(ddef1, specializedDecl)
  }

  /** Dispatch to specialized `apply`s in user code when available */
  override def transformApply(tree: Apply)(using Context) =
    tree match {
      case Apply(fun: NameTree, args) if fun.name == nme.apply && args.size <= 3 && fun.symbol.maybeOwner.isType =>
        val argTypes = fun.tpe.widen.firstParamTypes.map(_.widenSingleton.dealias)
        val retType  = tree.tpe.widenSingleton.dealias
        val isSpecializable =
          defn.isSpecializableFunction(
            fun.symbol.owner.asClass,
            argTypes,
            retType
          )
        if isSpecializable then
          val specializedApply = nme.apply.specializedFunction(retType, argTypes)
          val newSel = fun match
            case Select(qual, _) =>
              val qual1 = qual.tpe.widen match
                case defn.ByNameFunction(res) =>
                  // Need to cast to regular function, since specialized apply methods
                  // are not members of ContextFunction0. The cast will be eliminated in
                  // erasure.
                  qual.cast(defn.FunctionNOf(Nil, res))
                case _ =>
                  qual
              qual1.select(specializedApply)
            case _ =>
              (fun.tpe: @unchecked) match
                case TermRef(prefix: ThisType, name) =>
                  tpd.This(prefix.cls).select(specializedApply)
                case TermRef(prefix: NamedType, name) =>
                  tpd.ref(prefix).select(specializedApply)
          newSel.appliedToTermArgs(args)
        else tree
      case _ => tree
    }

  private def derivesFromFn012(cls: ClassSymbol)(using Context): Boolean =
    cls.baseClasses.exists { p =>
      p == defn.Function0 || p == defn.Function1 || p == defn.Function2
    }
}

object SpecializeFunctions:
  val name: String = "specializeFunctions"
  val description: String = "specialize Function{0,1,2} by replacing super with specialized super"
