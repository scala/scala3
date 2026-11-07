package dotty.tools.dotc.classpath

/**
 * Common methods related to package names represented as String
 */
private object PackageNameUtils {

  /** Like the full version below, when you only need the package name. */
  def separatePackageName(fullClassName: String): String =
    val lastDotIndex = fullClassName.lastIndexOf('.')
    if lastDotIndex == -1 then ClassPath.RootPackage
    else fullClassName.substring(0, lastDotIndex)

  /**
   * @param fullClassName full class name with package
   * @return (package, simple class name)
   */
  inline def separatePkgAndClassNames(fullClassName: String): (String, String) = {
    val lastDotIndex = fullClassName.lastIndexOf('.')
    if (lastDotIndex == -1)
      (ClassPath.RootPackage, fullClassName)
    else
      (fullClassName.substring(0, lastDotIndex), fullClassName.substring(lastDotIndex + 1))
  }

  /**
   * `true` if `packageDottedName` is a package directly nested in `inPackage`, for example:
   *   - `packageContains("scala", "scala.collection")`
   *   - `packageContains("", "scala")`
   */
  def packageContains(inPackage: String, packageDottedName: String): Boolean = {
    if (packageDottedName.contains("."))
      packageDottedName.startsWith(inPackage) && packageDottedName.lastIndexOf('.') == inPackage.length
    else inPackage == ""
  }
}
