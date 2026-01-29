package dotty.tools.pc

import java.nio.file.Paths

import scala.meta.internal.pc.ExtractMethodUtils
import scala.meta.pc.OffsetParams
import scala.meta.pc.RangeParams
import scala.meta.pc.SymbolSearch
import scala.meta.pc.reports.ReportContext
import scala.meta as m

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.DeepFolder
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.core.Types.PolyType
import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l

final class ExtractMethodProvider(
    range: RangeParams,
    extractionPos: OffsetParams,
    driver: InteractiveDriver,
    search: SymbolSearch,
    noIndent: Boolean
)(using ReportContext)
    extends ExtractMethodUtils:

  def extractMethod(): List[TextEdit] =
    val text = range.text().nn
    val uri = range.uri().nn
    val filePath = Paths.get(uri)
    val source = SourceFile.virtual(filePath.toString, text)
    driver.run(uri, source)
    val unit = driver.currentCtx.run.nn.units.head
    val pos = driver.sourcePosition(range).startPos
    val path =
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)
    given locatedCtx: Context =
      val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)
      Interactive.contextOfPath(path)(using newctx)
    val indexedCtx = IndexedContext(pos)(using locatedCtx)
    val printer =
      ShortenedTypePrinter(search, IncludeDefaultParam.Never)(using indexedCtx)
    def prettyPrint(tpe: Type) =
      def prettyPrintReturnType(tpe: Type): String =
        tpe match
          case mt: (MethodType | PolyType) =>
            prettyPrintReturnType(tpe.resultType)
          case tpe => printer.tpe(tpe)
      def printParams(params: List[Type]) =
        params match
          case p :: Nil => prettyPrintReturnType(p)
          case _ => s"(${params.map(prettyPrintReturnType).mkString(", ")})"
      if tpe.paramInfoss.isEmpty
      then prettyPrintReturnType(tpe)
      else
        val params = tpe.paramInfoss.map(printParams).mkString(" => ")
        s"$params => ${prettyPrintReturnType(tpe)}"

    def extractFromBlock(t: tpd.Tree): List[tpd.Tree] =
      t match
        case Block(stats, expr) =>
          (stats :+ expr).filter(stat => range.encloses(stat.sourcePos))
        case temp: Template[?] =>
          temp.body.filter(stat => range.encloses(stat.sourcePos))
        case other => List(other)

    def localRefs(
        ts: List[tpd.Tree],
        defnPos: SourcePosition,
        extractedPos: SourcePosition
    ): (List[Symbol], List[Symbol]) =
      def nonAvailable(sym: Symbol): Boolean =
        val symPos = sym.sourcePos
        symPos.exists && defnPos.contains(symPos) && !extractedPos
          .contains(symPos)
      def collectNames(symbols: Set[Symbol], tree: tpd.Tree): Set[Symbol] =
        tree match
          case id @ Ident(_) =>
            val sym = id.symbol
            if nonAvailable(sym) && (sym.isTerm || sym.isTypeParam)
            then symbols + sym
            else symbols
          case _ => symbols

      val traverser = new DeepFolder[Set[Symbol]](collectNames)
      val allSymbols = ts
        .foldLeft(Set.empty[Symbol])(traverser(_, _))

      val methodParams = allSymbols.toList.filter(_.isTerm)
      val methodParamTypes = methodParams
        .flatMap(p => p :: p.paramSymss.flatten)
        .map(_.info.typeSymbol)
        .filter(tp => nonAvailable(tp) && tp.isTypeParam)
        .distinct
      // Type parameter can be a type of one of the parameters or a type parameter in extracted code
      val typeParams =
        allSymbols.filter(_.isTypeParam) ++ methodParamTypes

      (
        methodParams.sortBy(_.decodedName),
        typeParams.toList.sortBy(_.decodedName)
      )
    end localRefs
    val optEnclosing =
      path.dropWhile(src => !src.sourcePos.encloses(range)) match
        case Nil => None
        case _ :: (app @ Apply(fun, args)) :: _ if args.exists(ImplicitParameters.isSyntheticArg(_)) => Some(app)
        case found :: _ => Some(found)

    val edits =
      for
        enclosing <- optEnclosing
        extracted = extractFromBlock(enclosing)
        head <- extracted.headOption
        expr <- extracted.lastOption
        shortenedPath =
          path.takeWhile(src => extractionPos.offset() <= src.sourcePos.start)
        stat = shortenedPath.lastOption.getOrElse(head)
      yield
        val defnPos = stat.sourcePos
        val extractedPos = head.sourcePos.withEnd(expr.sourcePos.end)
        val exprType = prettyPrint(expr.typeOpt.widen)
        val name =
          genName(indexedCtx.scopeSymbols.map(_.decodedName).toSet, "newMethod")
        val (allMethodParams, typeParams) =
          localRefs(extracted, stat.sourcePos, extractedPos)
        val (methodParams, implicitParams) = allMethodParams.partition(!_.isOneOf(Flags.GivenOrImplicit))
        def toParamText(params: List[Symbol]) =
          params.map(sym => s"${sym.decodedName}: ${prettyPrint(sym.info)}")
            .mkString(", ")
        val methodParamsText = toParamText(methodParams)
        val implicitParamsText = if implicitParams.nonEmpty then s"(given ${toParamText(implicitParams)})" else ""
        val typeParamsText = typeParams
          .map(_.decodedName) match
          case Nil => ""
          case params => params.mkString("[", ", ", "]")
        val exprParamsText = methodParams.map(_.decodedName).mkString(", ")
        val newIndent = stat.startPos.startColumnPadding
        val oldIndentLen = head.startPos.startColumnPadding.length()
        val toExtract =
          textToExtract(
            text,
            head.startPos.start,
            expr.endPos.end,
            newIndent,
            oldIndentLen
          )
        val (obracket, cbracket) =
          if noIndent && extracted.length > 1 then (" {", s"$newIndent}")
          else ("", "")
        val defText =
          s"def $name$typeParamsText($methodParamsText)$implicitParamsText: $exprType =$obracket\n${toExtract}\n$cbracket\n$newIndent"
        val replacedText = s"$name($exprParamsText)"
        List(
          new l.TextEdit(
            extractedPos.toLsp,
            replacedText
          ),
          new l.TextEdit(
            defnPos.startPos.toLsp,
            defText
          )
        )

    edits.getOrElse(Nil)
  end extractMethod
end ExtractMethodProvider
