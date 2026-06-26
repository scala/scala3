/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools.dotc.classpath

import dotty.tools.io.ClassPath
import dotty.tools.io.ClassPath.RootPackage

/**
 * Common methods related to package names represented as String
 */
object PackageNameUtils {

  /**
   * @param fullClassName full class name with package
   * @return (package, simple class name)
   */
  inline def separatePkgAndClassNames(fullClassName: String): (String, String) = {
    val lastDotIndex = fullClassName.lastIndexOf('.')
    if (lastDotIndex == -1)
      (RootPackage, fullClassName)
    else
      (fullClassName.substring(0, lastDotIndex).nn, fullClassName.substring(lastDotIndex + 1).nn)
  }

  def packagePrefix(inPackage: String): String = if (inPackage == RootPackage) "" else inPackage + "."

  /**
   * `true` if `packageDottedName` is a package directly nested in `inPackage`, for example:
   *   - `packageContains("scala", "scala.collection")`
   *   - `packageContains("", "scala")`
   */
  def packageContains(inPackage: String, packageDottedName: String) = {
    if (packageDottedName.contains("."))
      packageDottedName.startsWith(inPackage) && packageDottedName.lastIndexOf('.') == inPackage.length
    else inPackage == ""
  }

  def dirPathTrailingSlashJar(pkg: String): String =
    FileUtils.dirPathInJar(pkg) + "/"

  def dirPathTrailingSlash(pkg: String): String =
    if (java.io.File.separatorChar == '/')
      dirPathTrailingSlashJar(pkg)
    else
      FileUtils.dirPath(pkg) + java.io.File.separator

  def entryName(pkg: String, entry: String): String = {
    if (pkg == ClassPath.RootPackage) entry else {
      val builder = new java.lang.StringBuilder(pkg.length + 1 + entry.length)
      builder.append(pkg)
      builder.append('.')
      builder.append(entry)
      builder.toString
    }
  }
}
