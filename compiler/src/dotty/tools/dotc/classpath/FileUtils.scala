/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package dotty.tools
package dotc.classpath

import java.io.{FileFilter, File as JFile}
import java.net.URL
import dotty.tools.io.{AbstractFile, FileExtension}

/**
 * Common methods related to Java files and abstract files used in the context of classpath
 */
object FileUtils {
  extension (file: AbstractFile) {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.name)

    /**
     * Safe method returning a sequence containing one URL representing this file, when underlying file exists,
     * and returning given default value in other case
     */
    def toURLs(default: => Seq[URL] = Seq.empty): Seq[URL] =
      val url = file.toURL
      if (url == null) default else Seq(url)

    /**
     * Returns if there is an existing sibling `.tasty` file.
     */
    def hasSiblingTasty: Boolean =
      assert(file.ext.isClass, s"non-class: $file")
      file.resolveSibling(classNameToTasty(file.name)) != null
  }

  extension (file: JFile) {
    def isPackage: Boolean = file.isDirectory && mayBeValidPackage(file.getName)

    def isClass: Boolean = file.isFile && hasClassExtension

    def hasClassExtension: Boolean = file.getName.endsWith(FileExtension.Class.withDot)

    def isTasty: Boolean = file.isFile && file.getName.endsWith(FileExtension.Tasty.withDot)

    def isBestEffortTasty: Boolean = file.isFile && file.getName.endsWith(FileExtension.Betasty.withDot)


    /**
     * Returns if there is an existing sibling `.tasty` file.
     */
    def hasSiblingTasty: Boolean =
      assert(file.hasClassExtension, s"non-class: $file")
      val path = file.toPath
      val tastyPath = path.resolveSibling(classNameToTasty(file.getName))
      java.nio.file.Files.exists(tastyPath)

  }

  def stripSourceExtension(fileName: String): String =
    if endsSourceExtension(fileName) then stripExtension(fileName)
    else throw new FatalError("Unexpected source file ending: " + fileName)

  def endsSourceExtension(fileName: String): Boolean =
    ends(fileName, FileExtension.Scala.withDot) || ends(fileName, FileExtension.Java.withDot)

  def dirPath(forPackage: String): String = forPackage.replace('.', JFile.separatorChar)

  def dirPathInJar(forPackage: String): String = forPackage.replace('.', '/')

  inline private def ends (filename:String, suffix:String) = filename.endsWith(suffix) && filename.length > suffix.length

  def stripExtension(fileName: String): String =
    fileName.substring(0, fileName.lastIndexOf('.'))

  // probably it should match a pattern like [a-z_]{1}[a-z0-9_]* but it cannot be changed
  // because then some tests in partest don't pass
  def mayBeValidPackage(dirName: String): Boolean =
    (dirName != "META-INF") && (dirName != "") && (dirName.charAt(0) != '.')

  def mkFileFilter(f: JFile => Boolean): FileFilter = new FileFilter {
    def accept(pathname: JFile): Boolean = f(pathname)
  }

  /** Transforms a .class file name to a .tasty file name */
  private def classNameToTasty(fileName: String): String =
    val classOrModuleName = fileName.stripSuffix(".class")
    val className =
      if classOrModuleName.endsWith("$")
        && classOrModuleName != "Null$" // scala.runtime.Null$
        && classOrModuleName != "Nothing$" // scala.runtime.Nothing$
        // Special case for `object $` in Amonite.
        // This is an ad-hoc workaround for Amonite `object $`. See issue #19702
        // This definition is not valid Scala.
        && classOrModuleName != "$"
      then classOrModuleName.stripSuffix("$")
      else classOrModuleName
    className + FileExtension.Tasty.withDot
}
