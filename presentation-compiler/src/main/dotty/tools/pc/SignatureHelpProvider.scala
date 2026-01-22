package dotty.tools.pc

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.Signatures
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.printer.ShortenedTypePrinter
import dotty.tools.pc.printer.ShortenedTypePrinter.IncludeDefaultParam
import dotty.tools.pc.utils.InteractiveEnrichments.*
import org.eclipse.lsp4j as l

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Types.{MethodType, TypeVar}
import dotty.tools.dotc.util.Spans.Span

import scala.jdk.CollectionConverters.*
import scala.meta.pc.reports.ReportContext
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch

object SignatureHelpProvider:

  def signatureHelp(
      driver: InteractiveDriver,
      params: OffsetParams,
      search: SymbolSearch
  )(using ReportContext): l.SignatureHelp =
    val uri = params.uri().nn
    val text = params.text().nn
    val sourceFile = SourceFile.virtual(uri, text)
    driver.run(uri.nn, sourceFile)

    driver.compilationUnits.get(uri) match
      case Some(unit) =>

        val pos = driver.sourcePosition(params, isZeroExtent = false)
        val path = Interactive.pathTo(unit.tpdTree, pos.span)(using driver.currentCtx)

        val localizedContext = Interactive.contextOfPath(path)(using driver.currentCtx)
        val indexedContext = IndexedContext(pos)(using driver.currentCtx)

        given Context = localizedContext.fresh
          .setCompilationUnit(unit)
          .setPrinterFn(_ => ShortenedTypePrinter(search, IncludeDefaultParam.Never)(using indexedContext))

        val (paramN, callableN, alternatives) = Signatures.signatureHelp(path, pos.span)
        val refinedAlternatives = refineSignatures(alternatives, path, pos.span)(using driver.currentCtx)

        val infos = refinedAlternatives.flatMap: signature =>
          signature.denot.map(signature -> _)

        val signatureInfos = infos.map { case (signature, denot) =>
          search.symbolDocumentation(denot.symbol) match
            case Some(doc) =>
              withDocumentation(
                doc,
                signature,
                denot.symbol.is(Flags.JavaDefined)
              ).getOrElse(signature)
            case _ => signature

        }

        new l.SignatureHelp(
          signatureInfos.map(signatureToSignatureInformation).asJava,
          callableN,
          paramN
        )
      case _ => new l.SignatureHelp()
  end signatureHelp

  private def withDocumentation(
      info: SymbolDocumentation,
      signature: Signatures.Signature,
      isJavaSymbol: Boolean
  ): Option[Signatures.Signature] =
    val methodParams = info.parameters().nn.asScala
    val typeParams = info.typeParameters().nn.asScala

    def updateParams(params: List[Signatures.Param], typeParamIndex: Int, methodParamIndex: Int): List[Signatures.Param] =
      params match
        case (head: Signatures.MethodParam) :: tail =>
          val rest = updateParams(tail, typeParamIndex, methodParamIndex + 1)
          methodParams.lift(methodParamIndex) match
            case Some(paramDoc) =>
              val newName =
                if isJavaSymbol && head.name.startsWith("x$") then
                  paramDoc.displayName()
                else head.name
              head.copy(name = newName.nn, doc = Some(paramDoc.docstring.nn)) :: rest
            case _ => head :: rest
        case (head: Signatures.TypeParam) :: tail =>
          val rest = updateParams(tail, typeParamIndex + 1, methodParamIndex)
          typeParams.lift(typeParamIndex) match
            case Some(paramDoc) =>
              head.copy(doc = Some(paramDoc.docstring.nn)) :: rest
            case _ => head :: rest
        case _ => Nil

    def updateParamss(
        params: List[List[Signatures.Param]],
        typeParamIndex: Int,
        methodParamIndex: Int
    ): List[List[Signatures.Param]] =
      params match
        case Nil => Nil
        case head :: tail =>
          val updated = updateParams(head, typeParamIndex, methodParamIndex)
          val (nextTypeParamIndex, nextMethodParamIndex) = head match
            case (_: Signatures.MethodParam) :: _ => (typeParamIndex, methodParamIndex + head.size)
            case (_: Signatures.TypeParam) :: _ => (typeParamIndex + head.size, methodParamIndex)
            case _ => (typeParamIndex, methodParamIndex)
          updated :: updateParamss(tail, nextTypeParamIndex, nextMethodParamIndex)
    val updatedParams = updateParamss(signature.paramss, 0, 0)
    Some(signature.copy(doc = Some(info.docstring().nn), paramss = updatedParams))
  end withDocumentation

  private def signatureToSignatureInformation(
      signature: Signatures.Signature
  ): l.SignatureInformation =
    val paramInfoss = (signature.paramss.flatten).map(paramToParameterInformation)
    val paramLists =
      signature.paramss
        .map { paramList =>
          val labels = paramList.map(_.show)
          val isImplicit = paramList.exists:
            case p: Signatures.MethodParam => p.isImplicit
            case _ => false
          val prefix = if isImplicit then "using " else ""
          val isTypeParams = paramList.forall(_.isInstanceOf[Signatures.TypeParam]) && paramList.nonEmpty
          val wrap: String => String = label => if isTypeParams then
            s"[$label]"
          else
            s"($label)"
          wrap(labels.mkString(prefix, ", ", ""))
        }.mkString


    val returnTypeLabel = signature.returnType.map(t => s": $t").getOrElse("")
    val label = s"${signature.name}$paramLists$returnTypeLabel"
    val documentation = signature.doc.map(markupContent)
    val sig = new l.SignatureInformation(label)
    sig.setParameters(paramInfoss.asJava)
    documentation.foreach(sig.setDocumentation(_))
    sig
  end signatureToSignatureInformation

  /**
   * Convert `param` to `ParameterInformation`
   */
  private def paramToParameterInformation(
      param: Signatures.Param
  ): l.ParameterInformation =
    val documentation = param.doc.map(markupContent)
    val info = new l.ParameterInformation(param.show)
    documentation.foreach(info.setDocumentation(_))
    info

  private def markupContent(content: String): l.MarkupContent | Null =
    if content.isEmpty() then null
    else
      val markup = new l.MarkupContent
      markup.setKind("markdown")
      markup.setValue(content.trim())
      markup

  private def refineSignatures(
      help: List[Signatures.Signature],
      path: List[tpd.Tree],
      span: Span
  )(using Context): List[Signatures.Signature] =
    val enclosingApply = path.find {
      case tpd.Apply(fun, _) => !fun.span.contains(span)
      case _ => false
    }

    enclosingApply match
      case Some(tpd.Apply(fun, _)) =>
        help.map { signature =>
          val matches = signature.denot.exists(_.symbol == fun.symbol)
          if matches then
            val (head, actions) = unwind(fun)
            var currType = head.tpe.widenTermRefExpr
            var actionIndex = 0

            val newParamss = signature.paramss.map { paramList =>
              paramList match
                case (p: Signatures.MethodParam) :: _ =>
                  // Handle PolyTypes if they were skipped in signature but exist in type
                  while currType.isInstanceOf[dotty.tools.dotc.core.Types.PolyType] do
                    if actionIndex < actions.size && actions(actionIndex).isInstanceOf[tpd.TypeApply] then
                      val targs = actions(actionIndex).asInstanceOf[tpd.TypeApply].args.map(_.tpe)
                      currType = currType.appliedTo(targs)
                      actionIndex += 1
                    else
                      // Fallback: strip aliases or stop if strict matching fails
                      currType = currType.resultType

                  if currType.isInstanceOf[MethodType] then
                    val mt = currType.asInstanceOf[MethodType]
                    val paramNames = mt.paramNames.map(_.show)
                    val res = paramList.map {
                      case p: Signatures.MethodParam =>
                        val idx = paramNames.indexOf(p.name)
                        val shown = if idx >= 0 then mt.paramInfos(idx).widenTermRefExpr.show else p.tpe
                        if shown.contains("Any") || shown.contains("Nothing") || shown.contains("error") then p
                        else p.copy(tpe = shown)
                      case other => other
                    }
                    currType = mt.resultType
                    if actionIndex < actions.size && actions(actionIndex).isInstanceOf[tpd.Apply] then
                      actionIndex += 1
                    res
                  else paramList

                case (p: Signatures.TypeParam) :: _ =>
                  if currType.isInstanceOf[dotty.tools.dotc.core.Types.PolyType] then
                    if actionIndex < actions.size && actions(actionIndex).isInstanceOf[tpd.TypeApply] then
                      val targs = actions(actionIndex).asInstanceOf[tpd.TypeApply].args.map(_.tpe)
                      val newParams = paramList.zip(targs).map {
                        case (p: Signatures.TypeParam, targ) =>
                          val shown = targ.show
                          if shown.contains("Any") || shown.contains("Nothing") || shown.contains("error") then p
                          else p.copy(tpe = shown)
                        case (p, _) => p
                      }
                      currType = currType.appliedTo(targs)
                      actionIndex += 1
                      newParams
                    else paramList
                  else paramList

                case _ => paramList
            }
            // Update return type if we have consumed all actions and ended up with a result type
            val finalType = currType.widen.show
            val returnTypeLabel = 
              if actionIndex == actions.size && !finalType.contains("error") && !finalType.contains("Any") && !finalType.contains("Nothing") then Some(finalType)
              else signature.returnType
            
            signature.copy(paramss = newParamss, returnType = returnTypeLabel)
          else signature
        }
      case _ => help

  private def unwind(tree: tpd.Tree): (tpd.Tree, List[tpd.Tree]) =
    tree match
      case tpd.Apply(fn, _) =>
        val (head, actions) = unwind(fn)
        (head, actions :+ tree)
      case tpd.TypeApply(fn, _) =>
        val (head, actions) = unwind(fn)
        (head, actions :+ tree)
      case _ => (tree, Nil)

  private def countParams(tree: tpd.Tree): Int =
    tree match
      case tpd.Apply(fun, _) => 1 + countParams(fun)
      case tpd.TypeApply(fun, _) => 1 + countParams(fun)
      case _ => 0

end SignatureHelpProvider
