package dotty.tools.dotc
package transform

import ast.Trees._, ast.tpd, core._
import Contexts._, Types._, Decorators._, Symbols._, DenotTransformers._
import SymDenotations._, Scopes._, StdNames._, NameOps._, Names._
import MegaPhase.MiniPhase

import scala.collection.mutable


/** This phase synthesizes specialized methods for FunctionN, this is done
 *  since there are no scala signatures in the bytecode for the specialized
 *  methods.
 *
 *  We know which specializations exist for the different arities, therefore we
 *  can hardcode them. This should, however be removed once we're using a
 *  different standard library.
 */
class SpecializeApplyMethods extends MiniPhase with InfoTransformer {
  import ast.tpd._

  override def phaseName: String = SpecializeApplyMethods.name

  override def description: String = SpecializeApplyMethods.description

  override def isEnabled(using Context): Boolean =
    !ctx.settings.scalajs.value

  private def specApplySymbol(sym: Symbol, args: List[Type], ret: Type)(using Context): Symbol = {
    val name = nme.apply.specializedFunction(ret, args)
    // Create the symbol at the next phase, so that it is a valid member of the
    // corresponding function for all valid periods of its SymDenotations.
    // Otherwise, the valid period will offset by 1, which causes a stale symbol
    // in compiling stdlib.
    atNextPhase(newSymbol(sym, name, Flags.Method, MethodType(args, ret)))
  }

  private inline def specFun0(inline op: Type => Unit)(using Context): Unit = {
    for (r <- defn.Function0SpecializedReturnTypes) do
      op(r)
  }

  private inline def specFun1(inline op: (Type, Type) => Unit)(using Context): Unit = {
    for
      r  <- defn.Function1SpecializedReturnTypes
      t1 <- defn.Function1SpecializedParamTypes
    do
      op(t1, r)
  }

  private inline def specFun2(inline op: (Type, Type, Type) => Unit)(using Context): Unit = {
    for
      r  <- defn.Function2SpecializedReturnTypes
      t1 <- defn.Function2SpecializedParamTypes
      t2 <- defn.Function2SpecializedParamTypes
    do
      op(t1, t2, r)
  }

  override def infoMayChange(sym: Symbol)(using Context) =
    sym == defn.Function0
    || sym == defn.Function1
    || sym == defn.Function2

  /** Add symbols for specialized methods to FunctionN */
  override def transformInfo(tp: Type, sym: Symbol)(using Context) = tp match {
    case tp: ClassInfo =>
      if sym == defn.Function0 then
        val scope = tp.decls.cloneScope
        specFun0 { r => scope.enter(specApplySymbol(sym, Nil, r)) }
        tp.derivedClassInfo(decls = scope)

      else if sym == defn.Function1 then
        val scope = tp.decls.cloneScope
        specFun1 { (t1, r) => scope.enter(specApplySymbol(sym, t1 :: Nil, r)) }
        tp.derivedClassInfo(decls = scope)

      else if sym == defn.Function2 then
        val scope = tp.decls.cloneScope
        specFun2 { (t1, t2, r) => scope.enter(specApplySymbol(sym, t1 :: t2 :: Nil, r)) }
        tp.derivedClassInfo(decls = scope)

      else tp

    case _ => tp
  }

  /** Create bridge methods for FunctionN with specialized applys */
  override def transformTemplate(tree: Template)(using Context) = {
    val cls = tree.symbol.owner.asClass

    def synthesizeApply(names: collection.Set[TermName]): Tree = {
      val applyBuf = new mutable.ListBuffer[DefDef]
      names.foreach { name =>
        val applySym = cls.info.decls.lookup(name)
        val ddef = DefDef(
          applySym.asTerm,
          { vparamss =>
              This(cls)
                .select(nme.apply)
                .appliedToArgss(vparamss)
                .ensureConforms(applySym.info.finalResultType)
          }
        )
        applyBuf += ddef
      }
      cpy.Template(tree)(body = tree.body ++ applyBuf)
    }

    if cls == defn.Function0 then
      synthesizeApply(defn.Function0SpecializedApplyNames)
    else if cls == defn.Function1 then
      synthesizeApply(defn.Function1SpecializedApplyNames)
    else if cls == defn.Function2 then
      synthesizeApply(defn.Function2SpecializedApplyNames)
    else
      tree
  }
}

object SpecializeApplyMethods:
  val name: String = "specializeApplyMethods"
  val description: String = "adds specialized methods to FunctionN"
