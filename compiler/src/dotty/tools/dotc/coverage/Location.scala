package dotty.tools.dotc
package coverage

import ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import java.nio.file.Path
import dotty.tools.dotc.util.SourceFile

/** Information about the location of a coverable piece of code.
  *
  * @param packageName    name of the enclosing package
  * @param className      name of the closest enclosing class
  * @param fullClassName  fully qualified name of the closest enclosing class
  * @param classType      "type" of the closest enclosing class: Class, Trait or Object
  * @param methodName     name of the closest enclosing method
  * @param sourcePath     absolute path of the source file
  */
final case class Location(
    packageName: String,
    className: String,
    fullClassName: String,
    classType: String,
    methodName: String,
    sourcePath: Path
)

object Location:
  /** Extracts the location info of a Tree. */
  def apply(tree: Tree, source: SourceFile)(using ctx: Context): Location =

    val ownerDenot = ctx.owner.denot
    val enclosingClass = ownerDenot.enclosingClass
    val packageName = ownerDenot.enclosingPackageClass.fullName.toSimpleName.show
    val className = enclosingClass.name.toSimpleName.show
    val methodName = ownerDenot.enclosingMethod.name.toSimpleName.show

    val classType: String =
      if enclosingClass.is(Trait) then "Trait"
      else if enclosingClass.is(ModuleClass) then "Object"
      else "Class"

    Location(
      packageName,
      className,
      s"$packageName.$className",
      classType,
      methodName,
      source.file.absolute.jpath.nn
    )
