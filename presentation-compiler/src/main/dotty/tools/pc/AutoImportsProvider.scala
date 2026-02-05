package dotty.tools.pc

import java.nio.file.Paths

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.meta.internal.pc.AutoImportsResultImpl
import scala.meta.pc.*
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Flags.Method
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.completions.CompletionPos
import dotty.tools.pc.utils.InteractiveEnrichments.*

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

    val indexedContext = IndexedContext(pos)(
      using Interactive.contextOfPath(path)(using newctx)
    )
    import indexedContext.ctx

    def correctInTreeContext(sym: Symbol) = path match
      case (_: Ident) :: (sel: Select) :: _ =>
        sym.info.allMembers.exists(_.name == sel.name)
      case (_: Ident) :: (_: Apply) :: _ if !sym.is(Method) =>
        def applyInObject =
          sym.companionModule.info.allMembers.exists(_.name == nme.apply)
        def applyInClass = sym.info.allMembers.exists(_.name == nme.apply)
        applyInClass || applyInObject
      case _ => true

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
    val results = symbols.result().filter(isExactMatch(_, name))

    if results.nonEmpty then
      val correctedPos =
        CompletionPos.infer(pos, params, path, wasCursorApplied = false).toSourcePosition
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
                indexedContext,
                config
              )
            (sym: Symbol) => generator.forSymbol(sym)
        end match

      val all =
        for
          sym   <- results
          edits <- mkEdit(sym)
        yield (
          AutoImportsResultImpl(
            sym.owner.showFullName,
            edits.asJava
          ),
          sym
        )

      all match
        case (onlyResult, _) :: Nil => List(onlyResult)
        case Nil => Nil
        case moreResults =>
          val moreExact = moreResults.filter { case (_, sym) =>
            correctInTreeContext(sym)
          }
          if moreExact.nonEmpty then moreExact.map(_._1)
          else moreResults.map(_._1)
    else List.empty
    end if
  end autoImports

end AutoImportsProvider
