package dotty.tools
package dotc
package interactive


import ast.tpd
import core._
import Contexts._, NameOps._, Symbols._, StdNames._
import util._, util.Spans._

/**
 * A `tree` coming from `source`
 *
 * `tree` can be either an `Import` or a `NameTree`.
 */
case class SourceTree(tree: tpd.Import | tpd.NameTree, source: SourceFile) {

  /** The position of `tree` */
  final def pos(using Context): SourcePosition = source.atSpan(tree.span)

  /** The position of the name in `tree` */
  def namePos(using Context): SourcePosition = tree match {
    case tree: tpd.NameTree =>
      // FIXME: Merge with NameTree#namePos ?
      val treeSpan = tree.span
      if (treeSpan.isZeroExtent || tree.name.toTermName == nme.ERROR)
        NoSourcePosition
      else {
        // Constructors are named `<init>` in the trees, but `this` in the source.
        val nameLength = tree.name match {
          case nme.CONSTRUCTOR => nme.this_.toString.length
          case other => other.stripModuleClassSuffix.show.toString.length
        }
        val position = {
          // FIXME: This is incorrect in some cases, like with backquoted identifiers,
          //        see https://github.com/lampepfl/dotty/pull/1634#issuecomment-257079436
          val (start, end) =
            if (!treeSpan.isSynthetic)
              (treeSpan.point, treeSpan.point + nameLength)
            else
              // If we don't have a point, we need to find it
              (treeSpan.end - nameLength, treeSpan.end)
          Span(start, end, start)
        }
        source.atSpan(position)
      }
    case _ =>
      NoSourcePosition
  }
}

object SourceTree {
  def fromSymbol(sym: ClassSymbol, id: String = "")(using Context): List[SourceTree] =
    if (sym == defn.SourceFileAnnot || // FIXME: No SourceFile annotation on SourceFile itself
        !sym.source.exists) // FIXME: We cannot deal with external projects yet
      Nil
    else {
      import ast.Trees._
      def sourceTreeOfClass(tree: tpd.Tree): Option[SourceTree] = tree match {
        case PackageDef(_, stats) =>
          stats.flatMap(sourceTreeOfClass).headOption
        case tree: tpd.TypeDef if tree.symbol == sym =>
          Some(SourceTree(tree, sym.source))
        case _ =>
          None
      }

      def sourceImports(tree: tpd.Tree, sourceFile: SourceFile): List[SourceTree] = tree match {
        case PackageDef(_, stats) => stats.flatMap(sourceImports(_, sourceFile))
        case imp: tpd.Import => SourceTree(imp, sourceFile) :: Nil
        case _ => Nil
      }

      val tree = sym.rootTreeContaining(id)
      sourceTreeOfClass(tree) match {
        case Some(namedTree) => namedTree :: sourceImports(tree, namedTree.source)
        case None => Nil
      }
    }
}
