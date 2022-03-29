package dotty.tools.dotc
package coverage

import ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import java.nio.file.Path

/** Information about the location of a coverable piece of code.
  *
  * @param packageName    name of the enclosing package
  * @param className      name of the closest enclosing class
  * @param fullClassName  fully qualified name of the closest enclosing class
  * @param classType      "type" of the closest enclosing class: Class, Trait or Object
  * @param method name    of the closest enclosing method
  * @param sourcePath     absolute path of the source file
  */
final case class Location(
    packageName: String,
    className: String,
    fullClassName: String,
    classType: String,
    method: String,
    sourcePath: Path
)

object Location:
  /** Extracts the location info of a Tree. */
  def apply(tree: Tree)(using ctx: Context): Location =

    val enclosingClass = ctx.owner.denot.enclosingClass
    val packageName = ctx.owner.denot.enclosingPackageClass.name.toSimpleName.toString
    val className = enclosingClass.name.toSimpleName.toString

    val classType: String =
      if enclosingClass.is(Trait) then "Trait"
      else if enclosingClass.is(ModuleClass) then "Object"
      else "Class"

    Location(
      packageName,
      className,
      s"$packageName.$className",
      classType,
      ctx.owner.denot.enclosingMethod.name.toSimpleName.toString(),
      ctx.source.file.absolute.jpath
    )
