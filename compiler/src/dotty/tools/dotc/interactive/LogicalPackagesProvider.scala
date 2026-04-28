package dotty.tools.dotc.interactive

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.parsing.JavaParsers
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.AbstractFile
import dotty.tools.io.ClassPath
import dotty.tools.io.FileExtension
import dotty.tools.io.Path

import java.io.File
import scala.collection.mutable

/**
 * A compiler component that adds support for parsing Scala and Java source files and finding out
 * the logical package structure of the whole source path.
 */
class LogicalPackagesProvider(sourcePath: String)(using Context){

  private lazy val sourceRoots: Seq[SourceFile] =
    allSources(sourcePath).map(f => SourceFile(f, f.toCharArray))

  lazy val root: LogicalPackage = parseSourcePath()

  /**
   * Parse all source files in the sourcepath and build the logical package structure.
   */
  def parseSourcePath(): LogicalPackage =
    val pkg: ParsedLogicalPackage = newPackage()
    for sourceFile <- sourceRoots do
      try
        parseSourceFile(sourceFile, pkg)
      catch
        case e: Exception =>
          // Silently ignore parsing errors
    pkg

  private def newPackage(): ParsedLogicalPackage =
    new ParsedLogicalPackage("", None)

  private def parseSourceFile(
      sourceFile: SourceFile,
      rootPackage: ParsedLogicalPackage
  ): Unit =
    val fileName = sourceFile.path
    if fileName.endsWith(".scala") then
      parseScalaSourceFile(sourceFile, fileName, rootPackage)
    else if fileName.endsWith(".java") then
      parseJavaSourceFile(sourceFile, fileName, rootPackage)

  private def parseScalaSourceFile(
      sourceFile: SourceFile,
      fileName: String,
      rootPackage: ParsedLogicalPackage
  ): Unit =
    try
      // Use OutlineParser for fast parsing that skips method bodies
      val parser = new Parsers.OutlineParser(sourceFile)
      val tree = parser.parse()
      val traverser = new SourceFileTraverser(fileName, rootPackage)
      traverser.traverse(tree)
    catch
      case e: Exception =>
        // Silently ignore parsing errors

  private def parseJavaSourceFile(
      sourceFile: SourceFile,
      fileName: String,
      rootPackage: ParsedLogicalPackage
  ): Unit =
    try
      // Use OutlineJavaParser for fast parsing
      val parser = new JavaParsers.OutlineJavaParser(sourceFile)
      val tree = parser.parse()

      // Traverse the tree to extract package info
      val traverser = new SourceFileTraverser(fileName, rootPackage)
      traverser.traverse(tree)
    catch
      case e: Exception =>
        // Silently ignore parsing errors

  /**
   * Traverse an untyped AST to extract package and class definitions.
   */
  private class SourceFileTraverser(
      fileName: String,
      rootPackage: ParsedLogicalPackage
  ) {
    private var currentPackage = rootPackage

    def traverse(tree: untpd.Tree): Unit = tree match
      case untpd.Thicket(trees) =>
        trees.foreach(traverse)
      case pkg: untpd.PackageDef =>
        traversePackageDef(pkg)
      case _: untpd.MemberDef =>
        // Top-level class or object in default package
        currentPackage.enterSource(fileName)
      case _ =>

    private def traversePackageDef(pkg: untpd.PackageDef): Unit = {
      // Navigate to the package
      val pkgName = packageNameFromTree(pkg.pid)
      val newCurrentPackage = {
        var current = currentPackage
        for (part <- pkgName.split('.') if part.nonEmpty) {
          current = current.enterPackage(part)
        }
        current
      }
      // Traverse nested definitions
      val oldPackage = currentPackage
      currentPackage = newCurrentPackage
      pkg.stats.foreach(traverse)
      currentPackage = oldPackage
    }

    private def packageNameFromTree(tree: untpd.Tree): String = tree match {
      case untpd.Ident(name) =>
        if (name == nme.ROOT || name == nme.EMPTY_PACKAGE || name == nme.EMPTY) ""
        else name.toString
      case untpd.Select(qual, name) =>
        val qualName = packageNameFromTree(qual)
        if (qualName.isEmpty) name.toString
        else s"$qualName.${name.toString}"
      case _ =>
        ""
    }
  }

  /**
   * Return all Scala and Java sources from the given sourcepath string.
   */
  private def allSources(srcPath: String): Seq[AbstractFile] = {
    val entries = ClassPath.split(srcPath).map(Path(_))
    def isRelevantFile(path: Path) =
      path.ext == FileExtension.Scala || path.ext == FileExtension.Java
    // avoid using IO operation, assume standard extensions, Metals sends files so no sense checking for directories eagerly
    val rootDirs = entries.filter(f => !isRelevantFile(f))
    val rootFiles = for {
      e <- entries
      if isRelevantFile(e)
      f <- Option(AbstractFile.getFile(e))
    } yield f
    rootFiles ++ rootDirs.flatMap(dir => sourcesIn(AbstractFile.getDirectory(dir).nn, "scala", "java"))
  }

  /**
   * Recursively find all source files with given extensions in a directory.
   */
  private def sourcesIn(
      dir: AbstractFile,
      extensions: String*
  ): Seq[AbstractFile] =
    dir.iterator.toSeq.flatMap { file =>
      if (file.isDirectory) sourcesIn(file, extensions*)
      else if (extensions.exists(ext => file.name.endsWith(s".$ext"))) Seq(file)
      else Seq.empty[AbstractFile]
    }
}
