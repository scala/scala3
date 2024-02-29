package dotty.tools.pc

import java.nio.file.Paths

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.pc.AutoImportsResultImpl
import scala.meta.pc.*

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.AutoImports.*
import dotty.tools.pc.completions.CompletionPos
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.eclipse.lsp4j as l

final class AutoImportsProvider(
    search: SymbolSearch,
    driver: InteractiveDriver,
    name: String,
    params: OffsetParams,
    config: PresentationCompilerConfig,
    buildTargetIdentifier: String
)(using ReportContext):

  def autoImports(isExtension: Boolean): List[AutoImportsResult] =
    val uri = params.uri().nn
    val text = params.text().nn
    val filePath = Paths.get(uri)
    driver.run(uri, SourceFile.virtual(filePath.toString, text))

    val unit = driver.currentCtx.run.nn.units.head
    val tree = unit.tpdTree

    val pos = driver.sourcePosition(params)

    val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)
    val path =
      Interactive.pathTo(newctx.compilationUnit.tpdTree, pos.span)(using newctx)

    val indexedContext = IndexedContext(
      Interactive.contextOfPath(path)(using newctx)
    )
    import indexedContext.ctx

    val isSeen = mutable.Set.empty[String]
    val symbols = List.newBuilder[Symbol]
    def visit(sym: Symbol): Boolean =
      val name = sym.denot.fullName.show
      if !isSeen(name) then
        isSeen += name
        symbols += sym
        true
      else false
    def isExactMatch(sym: Symbol, query: String): Boolean =
      sym.name.show == query

    val visitor = new CompilerSearchVisitor(visit)
    if isExtension then
      search.searchMethods(name, buildTargetIdentifier, visitor)
    else search.search(name, buildTargetIdentifier, visitor)
    val results = symbols.result.filter(isExactMatch(_, name))

    if results.nonEmpty then
      val correctedPos = CompletionPos.infer(pos, params, path).toSourcePosition
      val mkEdit =
        path match
          // if we are in import section just specify full name
          case (_: Ident) :: (_: Import) :: _ =>
            (sym: Symbol) =>
              val nameEdit =
                new l.TextEdit(correctedPos.toLsp, sym.fullNameBackticked)
              Some(List(nameEdit))
          case _ =>
            val generator =
              AutoImports.generator(
                correctedPos,
                text,
                tree,
                unit.comments,
                indexedContext.importContext,
                config
              )
            (sym: Symbol) => generator.forSymbol(sym)
        end match
      end mkEdit

      for
        sym <- results
        edits <- mkEdit(sym)
      yield AutoImportsResultImpl(
        sym.owner.showFullName,
        edits.asJava
      )
    else List.empty
    end if
  end autoImports

end AutoImportsProvider
