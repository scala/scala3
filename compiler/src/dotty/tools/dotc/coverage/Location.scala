package dotty.tools.dotc
package coverage

import ast.tpd._
import dotty.tools.dotc.core.Contexts.Context

/** @param packageName
  *   the name of the encosing package
  * @param className
  *   the name of the closes enclosing class
  * @param fullClassName
  *   the fully qualified name of the closest enclosing class
  */
final case class Location(
    packageName: String,
    className: String,
    fullClassName: String,
    classType: String,
    method: String,
    sourcePath: String
)

object Location {
  def apply(tree: Tree)(using ctx: Context): Location = {

    val packageName = ctx.owner.denot.enclosingPackageClass.name.toSimpleName.toString()
    val className = ctx.owner.denot.enclosingClass.name.toSimpleName.toString()

    Location(
      packageName,
      className,
      s"$packageName.$className",
      "Class" /* TODO refine this further */,
      ctx.owner.denot.enclosingMethod.name.toSimpleName.toString(),
      ctx.source.file.absolute.toString()
    )
  }
}
