package main.dotty.tools.pc

import java.nio.file.Paths
import java.util as ju

import scala.jdk.CollectionConverters.*
import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.TypeName
import dotty.tools.dotc.core.Symbols.ClassSymbol
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile

import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.TextEdit

class PcConvertToEnum(
  driver: InteractiveDriver,
  params: OffsetParams
) {

  given Context = driver.currentCtx

  def convertToEnum: ju.List[TextEdit] =
    val uri = params.uri
    val filePath = Paths.get(uri)
    driver.run(uri, SourceFile.virtual(filePath.toString, params.text))
    val pos = driver.sourcePosition(params)
    Interactive.pathTo(driver.openedTrees(uri), pos) match
      case (t @ TypeDef(name, rhs: Template)) :: tail if t.symbol.exists && t.symbol.is(Flags.Sealed) =>
        val sealedClassSymbol = t.symbol.asClass
        val (implementations, companionTree) = collectImplementations(sealedClassSymbol)
        val useFewerBraces: Boolean =
          val checkForBracePosition = companionTree.map(_.span.end).getOrElse(t.span.end) - 1
          if(checkForBracePosition >= 0 && params.text().charAt(checkForBracePosition) == '}') false else true
        val indentString = " " * detectIndentation(t)
        val toReplace = new TextEdit(t.sourcePos.toLsp, makeReplaceText(name, rhs, implementations, indentString, useFewerBraces))
        val deleteTextEdits = toDelete(sealedClassSymbol, implementations, companionTree)
        (toReplace :: deleteTextEdits).asJava
      case _ => List.empty.asJava

  private def collectImplementations(sym: ClassSymbol)(using Context) = {
    val collector = new DeepFolder[(List[TypeDef], Option[TypeDef])]({
        case ((acc, companion), t @ TypeDef(_, _: Template)) if t.symbol.isClass && !t.symbol.is(Flags.Synthetic) && t.symbol.info.parents.map(_.typeSymbol).exists(_ == sym) =>
          (t :: acc, companion)
        case ((acc, None), t @TypeDef(_, _: Template)) if t.symbol.isClass && t.symbol.companionClass == sym =>
          (acc, Some(t))
        case (acc, _) => acc
      })

    val (trees, companion) = collector.apply((Nil, None), driver.compilationUnits(params.uri()).tpdTree)
    (trees.reverse, companion)
  }

  private def toDelete(sealedClassSymbol: ClassSymbol, implementations: List[TypeDef], companionTree: Option[TypeDef])(using Context) =
    val moduleClass = sealedClassSymbol.companionClass
    val deleteCompanionObject = companionTree.collect:
      case ct @ TypeDef(name, t: Template) if getRelevantBodyParts(t).forall(implementations.contains(_)) =>
        expandedPosition(ct)
    val toReexport =
      implementations.groupBy(_.symbol.owner).collect:
        case (symbol, trees) if symbol != moduleClass => trees.head
      .toList
    val toDelete = implementations.filterNot(toReexport.contains(_))
    deleteCompanionObject
      .map(_ :: toDelete.filter(_.symbol.owner != moduleClass).map(expandedPosition(_)))
      .getOrElse(toDelete.map(expandedPosition(_))).map(new TextEdit(_, ""))
      ++ toReexport.map(_.sourcePos.toLsp).map(new TextEdit(_, s"export ${sealedClassSymbol.name}.*"))

  private def makeReplaceText(name: TypeName, rhs: Template, implementations: List[TypeDef], baseIndent: String, useFewerBraces: Boolean): String =
    val constrString = showFromSource(rhs.constr)
    val (simpleCases, complexCases) =
      if constrString == ""
      then implementations.partition(_.rhs.asInstanceOf[Template].constr.paramss == List(Nil))
      else (Nil, implementations)
    val simpleCasesString = if simpleCases.isEmpty then "" else s"\n$baseIndent  case " + simpleCases.map(tdefName).mkString("", ", ", "")
    val complexCasesString =
      complexCases.map:
        case tdef @ TypeDef(_, t @ Template(constr, _, _, _)) =>
          val parentConstructorString =
            t.parents.filterNot(_.span.isZeroExtent).map(showFromSource).mkString(", ")
          s"\n$baseIndent  case ${tdefName(tdef)}${showFromSource(constr)} extends $parentConstructorString"
        case _ => ""
      .mkString
    val newRhs = getRelevantBodyParts(rhs).map(showFromSource).map(stat => s"\n$baseIndent  $stat").mkString
    val (begMarker, endMarker) = if useFewerBraces then (":", "") else (" {", s"\n$baseIndent}" )
    s"enum $name${constrString}$begMarker$newRhs$simpleCasesString$complexCasesString$endMarker"

  private def showFromSource(t: Tree): String =
    params.text().substring(t.sourcePos.start, t.sourcePos.end)

  private def getRelevantBodyParts(rhs: Template): List[Tree] =
    def isParamOrTypeParam(stat: Tree): Boolean = stat match
      case stat: ValDef => stat.symbol.is(Flags.ParamAccessor)
      case stat: TypeDef => stat.symbol.is(Flags.Param)
      case _ => false
    rhs.body.filterNot(stat => stat.span.isZeroExtent || stat.symbol.is(Flags.Synthetic) || isParamOrTypeParam(stat))

  private def expandedPosition(tree: Tree): l.Range =
    extendRangeToIncludeWhiteCharsAndTheFollowingNewLine(params.text().toCharArray())(tree.span.start, tree.span.end) match
      case (start, end) =>
        tree.source.atSpan(tree.span.withStart(start).withEnd(end)).toLsp

  private def detectIndentation(tree: TypeDef): Int =
    val text = params.text()
    var curr = tree.span.start
    var indent = 0
    while(curr >= 0 && text(curr) != '\n')
      if text(curr).isWhitespace then indent += 1
      else indent = 0
      curr -= 1
    indent

  private def tdefName(tdef: TypeDef) =
    if tdef.symbol.is(Flags.ModuleClass) then tdef.symbol.companionModule.name else tdef.name
}

object PcConvertToEnum:
  val codeActionId = "ConvertToEnum"
