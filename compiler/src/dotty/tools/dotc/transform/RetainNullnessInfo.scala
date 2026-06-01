package dotty.tools.dotc
package transform

import core.*
import MegaPhase.*
import Contexts.*
import Symbols.*
import Types.*
import StdNames.*
import Flags.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.util.Property

/** This phase rewrites calls to array constructors to newArray method in Dotty.runtime.Arrays module.
 *
 * It assummes that generic arrays have already been handled by typer(see Applications.convertNewGenericArray).
 * Additionally it optimizes calls to scala.Array.ofDim functions by replacing them with calls to newArray with specific dimensions
 */
class RetainNullnessInfo extends MiniPhase {
  import ast.tpd.*

  override def phaseName: String = RetainNullnessInfo.name

  override def description: String = RetainNullnessInfo.description

  def isNonNullType(tp: Type)(using Context) = {
    !(defn.NullType <:< tp) && (tp ne defn.UnitType) && !tp.isPrimitiveValueType
  }

  override def transformValDef(tree: ValDef)(using Context): Tree = {
    val sym = tree.symbol

    if(ctx.explicitNulls && isNonNullType(sym.info) && (sym.is(Param) || sym.isLocal)){
      sym.addAnnotation(defn.NonNullAnnot)
    }
    tree
  }

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val resType = tree.tpt.tpe

    if(ctx.explicitNulls && isNonNullType(resType)){
      tree.symbol.addAnnotation(defn.NonNullAnnot)
    }
    tree
  }
}

object RetainNullnessInfo:
  val name: String = "retainNullnessInfo"
  val description: String = "add annotations to symbols whose owner is non-null and is either a local variable, parameter field, or method parameter"