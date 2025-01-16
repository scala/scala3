package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core.*, core.Decorators.*
import MegaPhase.*
import Types.*, Contexts.*, DenotTransformers.*
import SymDenotations.*, Symbols.*, StdNames.*, Denotations.*

/** This phase rewrite PolyFunction subclasses to FunctionN subclasses
 *
 *      class Foo extends PolyFunction {
 *          def apply(x_1: P_1, ..., x_N: P_N): R = rhs
 *      }
 *   becomes:
 *      class Foo extends FunctionN {
 *          def apply(x_1: P_1, ..., x_N: P_N): R = rhs
 *      }
 */
class ElimPolyFunction extends MiniPhase with DenotTransformer {

  import tpd.*

  override def phaseName: String = ElimPolyFunction.name

  override def description: String = ElimPolyFunction.description

  override def runsAfter = Set(Erasure.name)

  override def changesParents: Boolean = true // Replaces PolyFunction by FunctionN

  override def transform(ref: SingleDenotation)(using Context) = ref match {
    case ref: ClassDenotation if ref.symbol != defn.PolyFunctionClass && ref.derivesFrom(defn.PolyFunctionClass) =>
      val cinfo = ref.classInfo
      val newParent = functionTypeOfPoly(cinfo)
      val newParents = cinfo.declaredParents.map(parent =>
        if (parent.typeSymbol == defn.PolyFunctionClass)
          newParent
        else
          parent
      )
      ref.copySymDenotation(info = cinfo.derivedClassInfo(declaredParents = newParents))
    case _ =>
      ref
  }

  def functionTypeOfPoly(cinfo: ClassInfo)(using Context): Type = {
    val applyMeth = cinfo.decls.lookup(nme.apply).info
    val arity = applyMeth.paramNamess.head.length
    defn.FunctionType(arity)
  }

  override def transformTemplate(tree: Template)(using Context): Tree = {
    val newParents = tree.parents.mapconserve(parent =>
      if (parent.tpe.typeSymbol == defn.PolyFunctionClass) {
        val cinfo = tree.symbol.owner.asClass.classInfo
        tpd.TypeTree(functionTypeOfPoly(cinfo))
      }
      else
        parent
    )
    cpy.Template(tree)(parents = newParents)
  }
}

object ElimPolyFunction {
  val name: String = "elimPolyFunction"
  val description: String = "rewrite PolyFunction subclasses to FunctionN subclasses"
}

