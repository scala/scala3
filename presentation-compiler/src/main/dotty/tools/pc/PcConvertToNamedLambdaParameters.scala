package dotty.tools.pc

import java.nio.file.Paths
import java.util as ju

import scala.jdk.CollectionConverters.*
import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.pc.utils.InteractiveEnrichments.*
import dotty.tools.pc.utils.TermNameInference.*

import org.eclipse.lsp4j as l

/** Facilitates the code action that converts a wildcard lambda to a lambda with
 *  named parameters e.g.
 *
 *  List(1, 2).map(<<_>> + 1) => List(1, 2).map(i => i + 1)
 */
final class PcConvertToNamedLambdaParameters(
    driver: InteractiveDriver,
    params: OffsetParams
):
  import PcConvertToNamedLambdaParameters.*

  def convertToNamedLambdaParameters: ju.List[l.TextEdit] =
    val uri = params.uri
    val filePath = Paths.get(uri)
    driver.run(
      uri,
      SourceFile.virtual(filePath.toString, params.text)
    )
    given newctx: Context = driver.localContext(params)
    val pos = driver.sourcePosition(params)
    val trees = driver.openedTrees(uri)
    val treeList = Interactive.pathTo(trees, pos)
    // Extractor for a lambda function (needs context, so has to be defined here)
    val LambdaExtractor = Lambda(using newctx)
    // select the most inner wildcard lambda
    val firstLambda = treeList.collectFirst {
      case LambdaExtractor(params, rhsFn) if params.forall(isWildcardParam) =>
        params -> rhsFn
    }

    firstLambda match
      case Some((params, lambda)) =>
        // avoid names that are either defined or referenced in the lambda
        val namesToAvoid = allDefAndRefNamesInTree(lambda)
        // compute parameter names based on the type of the parameter
        val computedParamNames: List[String] =
          params.foldLeft(List.empty[String]) { (acc, param) =>
            val name = singleLetterNameStream(param.tpe.typeSymbol.name.toString())
              .find(n => !namesToAvoid.contains(n) && !acc.contains(n))
            acc ++ name.toList
          }
        if computedParamNames.size == params.size then
          val paramReferenceEdits = params.zip(computedParamNames).flatMap { (param, paramName) =>
            val paramReferencePosition = findParamReferencePosition(param, lambda)
            paramReferencePosition.toList.map { pos =>
              val position = pos.toLsp
              val range = new l.Range(
                position.getStart(),
                position.getEnd()
              )
              new l.TextEdit(range, paramName)
            }
          }
          val paramNamesStr = computedParamNames.mkString(", ")
          val paramDefsStr =
            if params.size == 1 then paramNamesStr
            else s"($paramNamesStr)"
          val defRange = new l.Range(
            lambda.sourcePos.toLsp.getStart(),
            lambda.sourcePos.toLsp.getStart()
          )
          val paramDefinitionEdits = List(
            new l.TextEdit(defRange, s"$paramDefsStr => ")
          )
          (paramDefinitionEdits ++ paramReferenceEdits).asJava
        else
          List.empty.asJava
      case _ =>
        List.empty.asJava

end PcConvertToNamedLambdaParameters

object PcConvertToNamedLambdaParameters:
  val codeActionId = "ConvertToNamedLambdaParameters"

  class Lambda(using Context):
    def unapply(tree: tpd.Block): Option[(List[tpd.ValDef], tpd.Tree)] = tree match
      case tpd.Block(
            (ddef @ tpd.DefDef(_, tpd.ValDefs(params) :: Nil, _, body: tpd.Tree)) :: Nil,
            tpd.Closure(_, meth, _)
          )
          if ddef.symbol == meth.symbol =>
        params match
          case List(param) =>
            // lambdas with multiple wildcard parameters are represented as a single parameter function and a block with wildcard valdefs
            Some(multipleUnderscoresFromBody(param, body))
          case _ => Some(params -> body)
      case _ => None

  private def multipleUnderscoresFromBody(
      param: tpd.ValDef,
      body: tpd.Tree
  )(using Context): (List[tpd.ValDef], tpd.Tree) = body match
    case tpd.Block(defs, expr) if param.symbol.is(Flags.Synthetic) =>
      val wildcardParamDefs = defs.collect {
        case valdef: tpd.ValDef if isWildcardParam(valdef) => valdef
      }
      if wildcardParamDefs.size == defs.size then wildcardParamDefs -> expr
      else List(param) -> body
    case _ => List(param) -> body

  def isWildcardParam(param: tpd.ValDef)(using Context): Boolean =
    param.name.toString.startsWith("_$")

  def findParamReferencePosition(param: tpd.ValDef, lambda: tpd.Tree)(using Context): Option[SourcePosition] =
    var pos: Option[SourcePosition] = None
    object FindParamReference extends tpd.TreeTraverser:
      override def traverse(tree: tpd.Tree)(using Context): Unit =
        tree match
          case ident @ tpd.Ident(_) if ident.symbol == param.symbol =>
            pos = Some(tree.sourcePos)
          case _ =>
            traverseChildren(tree)
    FindParamReference.traverse(lambda)
    pos

  def allDefAndRefNamesInTree(tree: tpd.Tree)(using Context): List[String] =
    object FindDefinitionsAndRefs extends tpd.TreeAccumulator[List[String]]:
      override def apply(x: List[String], tree: tpd.Tree)(using Context): List[String] =
        tree match
          case tpd.DefDef(name, _, _, _) =>
            super.foldOver(x :+ name.toString, tree)
          case tpd.ValDef(name, _, _) =>
            super.foldOver(x :+ name.toString, tree)
          case tpd.Ident(name) =>
            super.foldOver(x :+ name.toString, tree)
          case _ =>
            super.foldOver(x, tree)
    FindDefinitionsAndRefs.foldOver(Nil, tree)

end PcConvertToNamedLambdaParameters
