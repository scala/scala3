package dotty.tools.dotc
package coverage

import ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.util.SourceFile

/** Information about the location of a coverable piece of code.
  *
  * @param packageName    name of the enclosing package
  * @param className      name of the closest enclosing class
  * @param fullClassName  fully qualified name of the closest enclosing class
  * @param classType      "type" of the closest enclosing class: Class, Trait or Object
  * @param methodName     name of the closest enclosing method
  * @param sourcePath     path of the source file relative to the source root
  */
final case class Location(
    packageName: String,
    className: String,
    fullClassName: String,
    classType: String,
    methodName: String,
    sourcePath: String
)

object Location:
  /** Extracts the location info of a Tree. */
  def apply(tree: Tree, source: SourceFile)(using ctx: Context): Location =

    val ownerDenot = ctx.owner.denot
    val enclosingClass = ownerDenot.enclosingClass
    val packageName = ownerDenot.enclosingPackageClass.fullName.toSimpleName.show
    val className = enclosingClass.name.toSimpleName.show
    val methodName = ownerDenot.enclosingMethod.name.toSimpleName.show

    def fullClassName(cls: Symbol): String =
      val outer = cls.owner.enclosingClass
      val name = cls.name.toSimpleName.show
      if !outer.exists || outer.is(Package) then s"$packageName.$name"
      else s"${fullClassName(outer)}.$name"

    val classType: String =
      if enclosingClass.is(Trait) then "Trait"
      else if enclosingClass.is(ModuleClass) then "Object"
      else "Class"

    Location(
      packageName,
      className,
      fullClassName(enclosingClass),
      classType,
      methodName,
      source.pathRelativeToSourceRoot
    )
