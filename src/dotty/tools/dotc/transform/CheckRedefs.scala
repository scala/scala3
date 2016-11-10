package dotty.tools.dotc
package transform

import TreeTransforms._
import core._
import DenotTransformers._, Contexts._, Definitions._, Decorators._
import SymDenotations._

/** Checks if the user is trying to illegally override synthetic classes,
  * primitives or their boxed equivalent
  */
class CheckRedefs extends MiniPhaseTransform with SymTransformer {
  import ast.tpd._

  override def phaseName = "checkRedefs"

  def transformSym(sym: SymDenotation)(implicit ctx: Context) = {
    val defn = ctx.definitions
    val fullName = sym.symbol.showFullName

    // Since symbols cannot be compared with `==` since they rely on hashes for
    // uniqueness, we compare the full names instead
    val syntheticClasses = defn.syntheticCoreClasses.map(_.showFullName)
    val primitives = defn.ScalaValueClasses().map(_.showFullName)
    val boxedPrimitives = defn.ScalaBoxedClasses().map(_.showFullName)

    val illegalOverride =
      if (syntheticClasses.contains(fullName))
        Some("synthetic")
      else if (primitives.contains(fullName))
        Some("primitive")
      else if (boxedPrimitives.contains(fullName))
        Some("boxed primitive")
      else
        None

    illegalOverride.foreach { overriden =>
      ctx.error(i"Cannot define symbol overriding $overriden `${sym.fullName}`", sym.symbol.pos)
    }

    sym
  }
}
