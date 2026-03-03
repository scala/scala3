package dotty.tools.pc

import java.nio.file.Paths

import scala.meta.internal.pc.CodeActionErrorMessages
import scala.meta.pc.OffsetParams

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j as l

final class ConvertToNamedArgumentsProvider(
    driver: InteractiveDriver,
    params: OffsetParams,
    argIndices: Set[Int]
):

  def convertToNamedArguments: Either[String, List[l.TextEdit]] =
    val uri = params.uri().nn
    val text = params.text().nn
    val filePath = Paths.get(uri)
    driver.run(uri, SourceFile.virtual(filePath.toString, text))

    val unit = driver.currentCtx.run.nn.units.head
    val newctx = driver.currentCtx.fresh.setCompilationUnit(unit)
    val pos = driver.sourcePosition(params)
    val trees = driver.openedTrees(uri)
    val tree = Interactive.pathTo(trees, pos)(using newctx).headOption

    def paramss(fun: tpd.Tree)(using Context): List[String] =
      fun.typeOpt match
        case m: MethodType => m.paramNamess.flatten.map(_.toString)
        case _ =>
          fun.symbol.rawParamss.flatten
            .filter(!_.isTypeParam)
            .map(_.nameBackticked)

    object FromNewApply:
      def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree])] =
        tree match
          case fun @ tpd.Select(tpd.New(_), _) =>
            Some((fun, Nil))
          case tpd.TypeApply(FromNewApply(fun, argss), _) =>
            Some(fun, argss)
          case tpd.Apply(FromNewApply(fun, argss), args) =>
            Some(fun, argss ++ args)
          case _ => None

    def edits(tree: Option[tpd.Tree])(using Context): Either[String, List[l.TextEdit]] =
      def makeTextEdits(fun: tpd.Tree, args: List[tpd.Tree]) =
        if fun.symbol.is(Flags.JavaDefined) then
          Left(CodeActionErrorMessages.ConvertToNamedArguments.IsJavaObject)
        else
          Right(
            args.zipWithIndex
              .zip(paramss(fun))
              .collect {
                case ((arg, index), param) if argIndices.contains(index) =>
                  val position = arg.sourcePos.toLsp
                  position.setEnd(position.getStart())
                  new l.TextEdit(position, s"$param = ")
              }
          )
      tree match
        case Some(t) =>
          t match
            case FromNewApply(fun, args) =>
              makeTextEdits(fun, args)
            case tpd.Apply(fun, args) =>
              makeTextEdits(fun, args)
            case _ => Right(Nil)
        case _ => Right(Nil)
    edits(tree)(using newctx)
  end convertToNamedArguments
end ConvertToNamedArgumentsProvider
