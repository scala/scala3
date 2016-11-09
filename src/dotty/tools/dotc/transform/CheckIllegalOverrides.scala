package dotty.tools.dotc
package transform

import TreeTransforms._
import core._
import Contexts._, Definitions._, Decorators._

/** Checks if the user is trying to illegally override synthetic classes,
  * primitives or their boxed equivalent
  */
class CheckIllegalOverrides extends MiniPhaseTransform {
  import ast.tpd._

  override def phaseName = "checkPhantom"

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo) = {
    val defn = ctx.definitions
    val tpe = tree.tpe
    val fullName = tree.symbol.showFullName

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
      ctx.error(i"Cannot define symbol overriding $overriden class `$tpe`", tree.pos)
    }

    tree
  }
}
