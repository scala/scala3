package dotty.tools.dotc.semanticdb

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.{semanticdb => s}


private[semanticdb] class SyntheticsExtractor:
  import Scala3.{_, given}

  val visited = collection.mutable.HashSet[Tree]()

  def tryFindSynthetic(tree: Tree)(using Context, SemanticSymbolBuilder, TypeOps): Option[s.Synthetic] =
    extension (synth: s.Synthetic)
      def toOpt: Some[s.Synthetic] = Some(synth)

    val forSynthetic = tree match // not yet supported (for synthetics)
      case tree: Apply if isForSynthetic(tree) => true
      case tree: TypeApply if isForSynthetic(tree) => true
      case _ => false

    if visited.contains(tree) || forSynthetic then None
    else
      tree match
        case tree: TypeApply
          if tree.span.isSynthetic &&
            tree.args.forall(arg => !arg.symbol.isDefinedInSource) &&
            !tree.span.isZeroExtent &&
            (tree.fun match {
              // for `Bar[Int]` of `class Foo extends Bar[Int]`
              // we'll have `TypeTree(Select(New(AppliedTypeTree(...))), List(Int))`
              // in this case, don't register `*[Int]` to synthetics as we already have `[Int]` in source.
              case Select(New(AppliedTypeTree(_, _)), _) => false

              // for `new SomeJavaClass[Int]()`
              // there will be a synthesized default getter
              // in addition to the source derived one.
              case Select(_, name) if name.is(NameKinds.DefaultGetterName) => false
              case Select(fun, _) if fun.symbol.name.isDynamic => false
              case _ => true
            }) =>
          visited.add(tree)
          val fnTree = tree.fun match
            // Something like `List.apply[Int](1,2,3)`
            case select @ Select(qual, _) if isSyntheticName(select) =>
              s.SelectTree(
                s.OriginalTree(range(qual.span, tree.source)),
                Some(select.toSemanticId)
              )
            case _ =>
              s.OriginalTree(
                range(tree.fun.span, tree.source)
              )
          val targs = tree.args.map(targ => targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren))
          s.Synthetic(
            range(tree.span, tree.source),
            s.TypeApplyTree(
              fnTree, targs
            )
          ).toOpt

        case tree: Apply
          if tree.args.nonEmpty &&
            tree.args.forall(arg =>
              arg.symbol.isOneOf(GivenOrImplicit) &&
              arg.span.isSynthetic
            ) =>
          s.Synthetic(
            range(tree.span, tree.source),
            s.ApplyTree(
              tree.fun.toSemanticOriginal,
              tree.args.map(_.toSemanticTree),
              SymbolInformation.Property.GIVEN.value
            )
          ).toOpt

        case tree: Apply
            if tree.fun.symbol.is(Implicit) ||
              (tree.fun.symbol.name == nme.apply && tree.fun.span.isSynthetic) =>
          val pos = range(tree.span, tree.source)
          s.Synthetic(
            pos,
            s.ApplyTree(
              tree.fun.toSemanticTree,
              arguments = List(
                s.OriginalTree(pos)
              )
            )
          ).toOpt

        case _ => None

  private given TreeOps: AnyRef with
    extension (tree: Tree)
      def toSemanticTree(using Context, SemanticSymbolBuilder, TypeOps): s.Tree =
        tree match
          case tree: Apply =>
            s.ApplyTree(
              tree.fun.toSemanticQualifierTree,
              tree.args.map(_.toSemanticTree)
            )
          case tree: TypeApply =>
            s.TypeApplyTree(
              tree.fun.toSemanticQualifierTree,
              tree.args.map { targ =>
                targ.tpe.toSemanticType(targ.symbol)(using LinkMode.SymlinkChildren)
              }
            )
          case tree: Ident => tree.toSemanticId
          case tree: Select => tree.toSemanticId
          case _ => s.Tree.defaultInstance

      def toSemanticQualifierTree(using Context, SemanticSymbolBuilder): s.Tree = tree match
        case sel @ Select(qual, _) if sel.symbol.owner != qual.symbol =>
          s.SelectTree(qual.toSemanticId, Some(sel.toSemanticId))
        case fun => fun.toSemanticId

      def toSemanticId(using Context, SemanticSymbolBuilder) =
        s.IdTree(tree.symbol.symbolName)

      def toSemanticOriginal(using Context) =
        s.OriginalTree(range(tree.span, tree.source))
  end TreeOps


  private def isForSynthetic(tree: Tree): Boolean =
    def isForComprehensionSyntheticName(select: Select): Boolean =
      select.span.toSynthetic == select.qualifier.span.toSynthetic && (
        select.name == nme.map ||
        select.name == nme.flatMap ||
        select.name == nme.withFilter ||
        select.name == nme.foreach
      )
    tree match
      case Apply(fun, _) => isForSynthetic(fun)
      case TypeApply(fun, _) => isForSynthetic(fun)
      case select: Select => isForComprehensionSyntheticName(select)
      case _ => false

  private def isSyntheticName(select: Select): Boolean =
    select.span.toSynthetic == select.qualifier.span.toSynthetic && (
      select.name == nme.apply ||
      select.name == nme.update ||
      select.name == nme.foreach ||
      select.name == nme.withFilter ||
      select.name == nme.flatMap ||
      select.name == nme.map ||
      select.name == nme.unapplySeq ||
      select.name == nme.unapply)

end SyntheticsExtractor
