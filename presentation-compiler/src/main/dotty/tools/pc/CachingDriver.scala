package dotty.tools.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util as ju

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters.*
import scala.meta.pc.SemanticdbFileManager
import scala.meta.pc.SourcePathMode

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.interactive.LogicalPackage
import dotty.tools.dotc.interactive.LogicalPackagesProvider
import dotty.tools.dotc.interactive.ParsedLogicalPackage
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.util.SourceFile

/** CachingDriver is a wrapper class that provides a compilation cache for
 *  InteractiveDriver. CachingDriver skips running compilation if
 *    - the target URI of `run` is the same as the previous target URI
 *    - the content didn't change since the last compilation.
 *
 *  This compilation cache enables Metals to skip compilation and re-use the
 *  typed tree under the situation like developers sequentially hover on the
 *  symbols in the same file without any changes.
 *
 *  Note: we decided to cache only if the target URI is the same as in the
 *  previous run because of `InteractiveDriver.currentCtx` that should return
 *  the context that refers to the last compiled source file. It would be ideal
 *  if we could update currentCtx even when we skip the compilation, but we
 *  struggled to do that. See the discussion
 *  https://github.com/scalameta/metals/pull/4225#discussion_r941138403 To avoid
 *  the complexity related to currentCtx, we decided to cache only when the
 *  target URI only if the same as the previous run.
 */
class CachingDriver private (
    override val settings: List[String],
    precomputedSourcePackages: Option[LogicalPackage]
) extends InteractiveDriver(settings, precomputedSourcePackages):

  private var lastCompiledURI: URI = uninitialized
  private var previousDiags = List.empty[Diagnostic]

  private def alreadyCompiled(uri: URI, content: Array[Char]): Boolean =
    compilationUnits.get(uri) match
      case Some(unit)
          if lastCompiledURI == uri &&
            ju.Arrays.equals(unit.source.content(), content) =>
        true
      case _ => false

  override def run(uri: URI, source: SourceFile): List[Diagnostic] =
    if !alreadyCompiled(uri, source.content) then previousDiags = super.run(uri, source)
    lastCompiledURI = uri
    previousDiags

end CachingDriver

object CachingDriver:
  def apply(
      settings: List[String],
      sourcePath: ju.function.Supplier[ju.List[Path]],
      semanticdbFileManager: SemanticdbFileManager,
      sourcePathMode: SourcePathMode
  ): CachingDriver =
    val precomputedSourcePackages = sourcePathMode match
      case SourcePathMode.DISABLED => None
      case SourcePathMode.PRUNED | SourcePathMode.FULL =>
        val sourcePathFiles = sourcePath.get().asScala.toSeq
        val logicalSourcePath = sourcePathFiles.mkString(File.pathSeparator)
        if sourcePathFiles.nonEmpty then Some(new LogicalPackagesProvider(logicalSourcePath).root) else None
      case SourcePathMode.MBT =>
        Some(ParsedLogicalPackage.fromMbtIndex(semanticdbFileManager.listAllPackages()))
    new CachingDriver(settings, precomputedSourcePackages)
