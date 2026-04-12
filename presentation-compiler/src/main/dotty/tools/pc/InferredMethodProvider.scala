package dotty.tools.pc

import java.nio.file.Paths

import scala.annotation.tailrec
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch
import scala.meta.pc.reports.ReportContext

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l

/** Tries to calculate edits needed to create a method that will fix missing
 *  symbol in all the places that it is possible such as:
 *    - apply inside method invocation `method(.., nonExistent(param), ...)` and
 *      `method(.., nonExistent, ...)`
 *    - method in val definition `val value: DefinedType = nonExistent(param)`
 *      and `val value: DefinedType = nonExistent`
 *    - simple method call `nonExistent(param)` and `nonExistent`
 *    - method call inside a container `container.nonExistent(param)` and
 *      `container.nonExistent`
 *
 *  @param params position and actual source
 *  @param driver Scala 3 interactive compiler driver
 *  @param config presentation compiler configuration
 *  @param symbolSearch symbol search
 */
final class InferredMethodProvider(
    params: OffsetParams,
    driver: InteractiveDriver,
    config: PresentationCompilerConfig,
    symbolSearch: SymbolSearch
)(using ReportContext):

  case class AdjustTypeOpts(
      text: String,
      adjustedEndPos: l.Position
  )

  def inferredMethodEdits(
      adjustOpt: Option[AdjustTypeOpts] = None
  ): List[TextEdit] =
    val uri = params.uri().nn
    val filePath = Paths.get(uri).nn

    val sourceText = adjustOpt.map(_.text).getOrElse(params.text().nn)
    val source =
      SourceFile.virtual(filePath.toString(), sourceText)
    driver.run(uri, source)
    val unit = driver.currentCtx.run.nn.units.head
    val pos = driver.sourcePosition(params)
    val path =
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)

    given locatedCtx: Context = driver.localContext(params)
    val indexedCtx = IndexedContext(pos)(using locatedCtx)

    val autoImportsGen = AutoImports.generator(
      pos,
      sourceText,
      unit.tpdTree,
      unit.comments,
      indexedCtx,
      config
    )

    val printer = ShortenedTypePrinter(
      symbolSearch,
      includeDefaultParam = IncludeDefaultParam.ResolveLater,
      isTextEdit = true
    )(using indexedCtx)

    def imports: List[TextEdit] =
      printer.imports(autoImportsGen)

    def printType(tpe: Type): String =
      printer.tpe(tpe)

    def printName(name: Name): String =
      printer.nameString(name)

    def printParams(params: List[Type], startIndex: Int = 0): String =
      params.zipWithIndex
        .map { case (p, index) =>
          s"arg${index + startIndex}: ${printType(p)}"
        }
        .mkString(", ")

    def printSignature(
        methodName: Name,
        params: List[List[Type]],
        retTypeOpt: Option[Type]
    ): String =
      val retTypeString = retTypeOpt match
        case Some(retType) =>
          val printRetType = printType(retType)
          if retType.isAny then ""
          else s": $printRetType"
        case _ => ""

      val (paramsString, _) = params.foldLeft(("", 0)) {
        case ((acc, startIdx), paramList) =>
          val printed = s"(${printParams(paramList, startIdx)})"
          (acc + printed, startIdx + paramList.size)
      }

      s"def ${printName(methodName)}$paramsString$retTypeString = ???"

    @tailrec
    def countIndent(text: String, index: Int, acc: Int): Int =
      if index > 0 && text(index) != '\n' then countIndent(text, index - 1, acc + 1)
      else acc

    def indentation(text: String, pos: Int): String =
      if pos > 0 then
        val isSpace = text(pos) == ' '
        val isTab = text(pos) == '\t'
        val indent = countIndent(params.text(), pos, 0)

        if isSpace then " " * indent else if isTab then "\t" * indent else ""
      else ""

    def insertPosition() =
      val blockOrTemplateIndex =
        path.tail.indexWhere {
          case _: Block | _: Template => true
          case _ => false
        }
      path(blockOrTemplateIndex).sourcePos

    /** Returns the position to insert the method signature for a container. If
     *  the container has an empty body, the position is the end of the
     *  container. If the container has a non-empty body, the position is the
     *  end of the last element in the body.
     *
     *  @param container the container to insert the method signature for
     *  @return the position to insert the method signature for the container
     *    and a boolean indicating if the container has an empty body
     */
    def insertPositionFor(container: Tree): Option[(SourcePosition, Boolean)] =
      val typeSymbol = container.tpe.widenDealias.typeSymbol
      if typeSymbol.exists then
        val trees = driver.openedTrees(params.uri().nn)
        val include = Interactive.Include.definitions | Interactive.Include.local
        Interactive.findTreesMatching(trees, include, typeSymbol).headOption match
          case Some(srcTree) =>
            srcTree.tree match
              case classDef: TypeDef if classDef.rhs.isInstanceOf[Template] =>
                val template = classDef.rhs.asInstanceOf[Template]
                val (pos, hasEmptyBody) = template.body.lastOption match
                  case Some(last) => (last.sourcePos, false)
                  case None => (classDef.sourcePos, true)
                Some((pos, hasEmptyBody))
              case _ => None
          case None => None
      else None

      /** Extracts type information for a specific parameter in a method
       *  signature. If the parameter is a function type, extracts both the
       *  function's argument types and return type. Otherwise, extracts just
       *  the parameter type.
       *
       *  @param methodType the method type to analyze
       *  @param argIndex the index of the parameter to extract information for
       *  @return
       *    a tuple of (argument types, return type) where:
       *    - argument types: Some(List[Type]) if parameter is a function, None
       *      otherwise
       *    - return type: Some(Type) representing either the function's return
       *      type or the parameter type itself
       */
    def extractParameterTypeInfo(methodType: Type, argIndex: Int): (Option[List[Type]], Option[Type]) =
      methodType match
        case m @ MethodType(param) =>
          val expectedFunctionType = m.paramInfos(argIndex)
          if defn.isFunctionType(expectedFunctionType) then
            expectedFunctionType match
              case defn.FunctionOf(argTypes, retType, _) =>
                (Some(argTypes), Some(retType))
              case _ =>
                (None, Some(expectedFunctionType))
          else
            (None, Some(m.paramInfos(argIndex)))
        case _ => (None, None)

    def signatureEdits(signature: String): List[TextEdit] =
      val pos = insertPosition()
      val indent = indentation(params.text(), pos.start - 1)
      val lspPos = pos.toLsp
      lspPos.setEnd(lspPos.getStart())

      List(
        TextEdit(
          lspPos,
          s"$signature\n$indent"
        )
      ) ::: imports

    def signatureEditsForContainer(signature: String, container: Tree): List[TextEdit] =
      insertPositionFor(container) match
        case Some((pos, hasEmptyBody)) =>
          val lspPos = pos.toLsp
          lspPos.setStart(lspPos.getEnd())
          val indent = indentation(params.text(), pos.start - 1)

          if hasEmptyBody then
            List(
              TextEdit(
                lspPos,
                s":\n  $indent$signature"
              )
            ) ::: imports
          else
            List(
              TextEdit(
                lspPos,
                s"\n$indent$signature"
              )
            ) ::: imports
        case None =>
          extensionMethodEdits(signature, container)

    def extensionMethodEdits(signature: String, container: Tree): List[TextEdit] =
      val containerTypeStr = printType(container.tpe.widenDealias)

      val pos = insertPosition()
      val indent = indentation(params.text(), pos.start - 1)
      val extensionSignature = s"extension (x: $containerTypeStr)\n  $indent$signature"

      val lspPos = pos.toLsp
      lspPos.setEnd(lspPos.getStart())

      List(
        TextEdit(
          lspPos,
          s"$extensionSignature\n$indent"
        )
      ) ::: imports

    path match
      /** ```
       *                  outerArgs
       *         ---------------------------
       *  method(..., errorMethod(args), ...)
       *  ```
       */
      case (id @ Ident(errorMethod)) ::
          (apply @ Apply(func, args)) ::
          Apply(method, outerArgs) ::
          _ if id.symbol == NoSymbol && func == id && method != apply =>

        val argTypes = args.map(_.typeOpt.widenDealias)

        val argIndex = outerArgs.indexOf(apply)
        val (allArgTypes, retTypeOpt) =
          extractParameterTypeInfo(method.tpe.widenDealias, argIndex) match
            case (Some(argTypes2), retTypeOpt) => (List(argTypes, argTypes2), retTypeOpt)
            case (None, retTypeOpt) => (List(argTypes), retTypeOpt)

        val signature = printSignature(errorMethod, allArgTypes, retTypeOpt)

        signatureEdits(signature)

      /** ```
       *              outerArgs
       *         ---------------------
       *  method(..., errorMethod, ...)
       *  ```
       */
      case (id @ Ident(errorMethod)) ::
          Apply(method, outerArgs) ::
          _ if id.symbol == NoSymbol && method != id =>

        val argIndex = outerArgs.indexOf(id)

        val (argTypes, retTypeOpt) = extractParameterTypeInfo(method.tpe.widenDealias, argIndex)

        val allArgTypes = argTypes match
          case Some(argTypes) => List(argTypes)
          case None => Nil

        val signature = printSignature(errorMethod, allArgTypes, retTypeOpt)

        signatureEdits(signature)

      /** ```
       *              outerArgs
       *         ---------------------
       *  method(..., errorMethod, ...)
       *  ```
       */
      case (id @ Ident(errorMethod)) ::
          (apply @ Apply(func, args)) ::
          ValDef(_, tpt, body) ::
          _ if id.symbol == NoSymbol && func == id && apply == body =>

        val retType = tpt.tpe.widenDealias
        val argTypes = args.map(_.typeOpt.widenDealias)

        val signature = printSignature(errorMethod, List(argTypes), Some(retType))
        signatureEdits(signature)

      /** ```
       *                 tpt          body
       *             -----------   -----------
       *  val value: DefinedType = errorMethod
       *
       *  ```
       */
      case (id @ Ident(errorMethod)) ::
          ValDef(_, tpt, body) ::
          _ if id.symbol == NoSymbol && id == body =>

        val retType = tpt.tpe.widenDealias

        val signature = printSignature(errorMethod, Nil, Some(retType))
        signatureEdits(signature)

      /** ```
       *  errorMethod(args)
       *  ```
       */
      case (id @ Ident(errorMethod)) ::
          (apply @ Apply(func, args)) ::
          _ if id.symbol == NoSymbol && func == id =>

        val argTypes = args.map(_.typeOpt.widenDealias)

        val signature = printSignature(errorMethod, List(argTypes), None)
        signatureEdits(signature)

      /** ```
       *  errorMethod
       *  ```
       */
      case (id @ Ident(errorMethod)) ::
          _ if id.symbol == NoSymbol =>

        val signature = printSignature(errorMethod, Nil, None)
        signatureEdits(signature)

      /** ```
       *  container.errorMethod(args)
       *  ```
       */
      case (select @ Select(container, errorMethod)) ::
          (apply @ Apply(func, args)) ::
          _ if select.symbol == NoSymbol && func == select =>

        val argTypes = args.map(_.typeOpt.widenDealias)
        val signature = printSignature(errorMethod, List(argTypes), None)
        signatureEditsForContainer(signature, container)

      /** ```
       *  container.errorMethod
       *  ```
       */
      case (select @ Select(container, errorMethod)) ::
          _ if select.symbol == NoSymbol =>

        val signature = printSignature(errorMethod, Nil, None)
        signatureEditsForContainer(signature, container)

      case _ => Nil

  end inferredMethodEdits
end InferredMethodProvider
