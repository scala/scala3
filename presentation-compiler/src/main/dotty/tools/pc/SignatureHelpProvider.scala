package dotty.tools.pc

import scala.jdk.CollectionConverters._
import scala.meta.pc.OffsetParams
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch

import dotty.tools.dotc.ast.Trees.AppliedTypeTree
import dotty.tools.dotc.ast.Trees.TypeApply
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.Signatures
import dotty.tools.dotc.util.Signatures.Signature
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.MtagsEnrichments.*

import org.eclipse.lsp4j as l

object SignatureHelpProvider:

  def signatureHelp(
      driver: InteractiveDriver,
      params: OffsetParams,
      search: SymbolSearch
  ) =
    val uri = params.uri()
    val sourceFile = SourceFile.virtual(params.uri().nn, params.text().nn)
    driver.run(uri.nn, sourceFile)

    given ctx: Context = driver.currentCtx

    val pos = driver.sourcePosition(params)
    val trees = driver.openedTrees(uri.nn)

    val path =
      Interactive.pathTo(trees, pos).dropWhile(t => notCurrentApply(t, pos))

    val (paramN, callableN, alternatives) =
      Signatures.signatureHelp(path, pos.span)
    val infos = alternatives.flatMap { signature =>
      signature.denot.map {
        (signature, _)
      }
    }

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

    /* Versions prior to 3.2.1 did not support type parameters
     * so we need to skip them.
     */
    new l.SignatureHelp(
      signatureInfos.map(signatureToSignatureInformation).asJava,
      callableN,
      paramN
    )
  end signatureHelp

  private def isValid(tree: tpd.Tree)(using Context): Boolean =
    ctx.definitions.isTupleClass(
      tree.symbol.owner.companionClass
    ) || ctx.definitions.isFunctionType(tree.tpe)

  private def notCurrentApply(
      tree: tpd.Tree,
      pos: SourcePosition
  )(using Context): Boolean =
    tree match
      case unapply: tpd.UnApply =>
        unapply.fun.span.contains(pos.span) || isValid(unapply)
      case typeTree @ AppliedTypeTree(fun, _) =>
        fun.span.contains(pos.span) || isValid(typeTree)
      case typeApply @ TypeApply(fun, _) =>
        fun.span.contains(pos.span) || isValid(typeApply)
      case appl: tpd.GenericApply =>
        /* find first apply that the cursor is located in arguments and not at function name
         * for example in:
         *   `Option(1).fold(2)(@@_ + 1)`
         * we want to find the tree responsible for the entire location, not just `_ + 1`
         */
        appl.fun.span.contains(pos.span)

      case _ => true

  private def withDocumentation(
      info: SymbolDocumentation,
      signature: Signatures.Signature,
      isJavaSymbol: Boolean
  ): Option[Signature] =
    val allParams = info.parameters().nn.asScala
    def updateParams(
        params: List[Signatures.Param],
        index: Int
    ): List[Signatures.Param] =
      params match
        case Nil => Nil
        case head :: tail =>
          val rest = updateParams(tail, index + 1)
          allParams.lift(index) match
            case Some(paramDoc) =>
              val newName =
                if isJavaSymbol && head.name.startsWith("x$") then
                  paramDoc.nn.displayName()
                else head.name
              head.copy(
                doc = Some(paramDoc.docstring.nn),
                name = newName.nn
              ) :: rest
            case _ => head :: rest

    def updateParamss(
        params: List[List[Signatures.Param]],
        index: Int
    ): List[List[Signatures.Param]] =
      params match
        case Nil => Nil
        case head :: tail =>
          val updated = updateParams(head, index)
          updated :: updateParamss(tail, index + head.size)
    val updatedParams = updateParamss(signature.paramss, 0)
    Some(signature.copy(doc = Some(info.docstring().nn), paramss = updatedParams))
  end withDocumentation

  private def signatureToSignatureInformation(
      signature: Signatures.Signature
  ): l.SignatureInformation =
    val tparams = signature.tparams.map(Signatures.Param("", _))
    val paramInfoss =
      (tparams ::: signature.paramss.flatten).map(paramToParameterInformation)
    val paramLists =
      if signature.paramss.forall(_.isEmpty) && tparams.nonEmpty then ""
      else
        signature.paramss
          .map { paramList =>
            val labels = paramList.map(_.show)
            val prefix = if paramList.exists(_.isImplicit) then "using " else ""
            labels.mkString(prefix, ", ", "")
          }
          .mkString("(", ")(", ")")
    val tparamsLabel =
      if signature.tparams.isEmpty then ""
      else signature.tparams.mkString("[", ", ", "]")
    val returnTypeLabel = signature.returnType.map(t => s": $t").getOrElse("")
    val label = s"${signature.name}$tparamsLabel$paramLists$returnTypeLabel"
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

end SignatureHelpProvider
