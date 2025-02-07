package dotty.tools.pc

import java.nio.file.Paths
import java.util.ArrayList

import scala.jdk.CollectionConverters.*
import scala.meta.internal.pc.DefinitionResultImpl
import scala.meta.pc.DefinitionResult
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.NavigateAST
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.{Exported, ModuleClass}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.Location

class PcDefinitionProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
    search: SymbolSearch
):

  def definitions(): DefinitionResult =
    definitions(findTypeDef = false)

  def typeDefinitions(): DefinitionResult =
    definitions(findTypeDef = true)

  private def definitions(findTypeDef: Boolean): DefinitionResult =
    val uri = params.uri().nn
    val text = params.text().nn
    val filePath = Paths.get(uri)
    driver.run(
      uri,
      SourceFile.virtual(filePath.toString, text)
    )

    val pos = driver.sourcePosition(params)
    val path =
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)

    given ctx: Context = driver.localContext(params)
    val indexedContext = IndexedContext(ctx)
    val result =
      if findTypeDef then findTypeDefinitions(path, pos, indexedContext)
      else findDefinitions(path, pos, indexedContext)

    if result.locations().nn.isEmpty() then fallbackToUntyped(pos)(using ctx)
    else result
  end definitions

  /**
   * Some nodes might disapear from the typed tree, since they are mostly
   * used as syntactic sugar. In those cases we check the untyped tree
   * and try to get the symbol from there, which might actually be there,
   * because these are the same nodes that go through the typer.
   *
   * This will happen for:
   * - `.. derives Show`
   * @param unit compilation unit of the file
   * @param pos cursor position
   * @return definition result
   */
  private def fallbackToUntyped(pos: SourcePosition)(
    using ctx: Context
  ) =
    lazy val untpdPath = NavigateAST
      .untypedPath(pos.span)
      .collect { case t: untpd.Tree => t }

    definitionsForSymbol(untpdPath.headOption.map(_.symbol).toList, pos)
  end fallbackToUntyped

  private def findDefinitions(
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext
  ): DefinitionResult =
    import indexed.ctx
    definitionsForSymbol(
      MetalsInteractive.enclosingSymbols(path, pos, indexed),
      pos
    )
  end findDefinitions

  private def findTypeDefinitions(
      path: List[Tree],
      pos: SourcePosition,
      indexed: IndexedContext
  ): DefinitionResult =
    import indexed.ctx
    val enclosing = path.expandRangeToEnclosingApply(pos)
    val typeSymbols = MetalsInteractive
      .enclosingSymbolsWithExpressionType(enclosing, pos, indexed)
      .map { case (_, tpe) =>
        tpe.typeSymbol
      }
    typeSymbols match
      case Nil =>
        path.headOption match
          case Some(value: Literal) =>
            definitionsForSymbol(List(value.typeOpt.widen.typeSymbol), pos)
          case _ => DefinitionResultImpl.empty
      case _ =>
        definitionsForSymbol(typeSymbols, pos)

  end findTypeDefinitions

  private def definitionsForSymbol(
      symbols: List[Symbol],
      pos: SourcePosition
  )(using ctx: Context): DefinitionResult =
    symbols match
      case symbols @ (sym :: other) =>
        val isLocal = sym.source == pos.source
        if isLocal then
          val (exportedDefs, otherDefs) =
            Interactive.findDefinitions(List(sym), driver, false, false)
              .filter(_.source == sym.source)
              .partition(_.tree.symbol.is(Exported))

          otherDefs.headOption.orElse(exportedDefs.headOption)  match
            case Some(srcTree) =>
              val pos = srcTree.namePos
              if pos.exists then
                  val loc = new Location(params.uri().toString(), pos.toLsp)
                  DefinitionResultImpl(
                    SemanticdbSymbols.symbolName(sym),
                    List(loc).asJava,
                  )
              else DefinitionResultImpl.empty
            case None =>
              DefinitionResultImpl.empty
        else
          val res = new ArrayList[Location]()
          semanticSymbolsSorted(symbols)
            .foreach { sym =>
              res.addAll(search.definition(sym, params.uri()))
            }
          DefinitionResultImpl(
            SemanticdbSymbols.symbolName(sym),
            res
          )
        end if
      case Nil => DefinitionResultImpl.empty
    end match
  end definitionsForSymbol

  def semanticSymbolsSorted(
      syms: List[Symbol]
  )(using ctx: Context): List[String] =
    syms
      .map { sym =>
        // in case of having the same type and teerm symbol
        // term comes first
        // used only for ordering symbols that come from `Import`
        val termFlag =
          if sym.is(ModuleClass) then sym.sourceModule.isTerm
          else sym.isTerm
        (termFlag, SemanticdbSymbols.symbolName(sym))
      }
      .sorted
      .map(_._2)

end PcDefinitionProvider
