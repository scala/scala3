package dotty.tools
package dotc
package interactive

import scala.io.Codec

import ast.tpd
import core._, core.Decorators.{sourcePos => _}
import Contexts._, NameOps._, Symbols._, StdNames._
import util._, util.Positions._

/** A `tree` coming from `source` */
sealed trait SourceTree {

  /** The underlying tree. */
  def tree: tpd.Tree

  /** The source from which `tree` comes. */
  def source: SourceFile

  /** The position of `tree` */
  final def pos(implicit ctx: Context): SourcePosition = source.atPos(tree.pos)
}

/** An import coming from `source` */
case class SourceImportTree(tree: tpd.Import, source: SourceFile) extends SourceTree

/** A typechecked `tree` coming from `source` */
case class SourceNamedTree(tree: tpd.NameTree, source: SourceFile) extends SourceTree {

  /** The position of the name in `tree` */
  def namePos(implicit ctx: Context): SourcePosition = {
    // FIXME: Merge with NameTree#namePos ?
    val treePos = tree.pos
    if (treePos.isZeroExtent || tree.name.toTermName == nme.ERROR)
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
          if (!treePos.isSynthetic)
            (treePos.point, treePos.point + nameLength)
          else
            // If we don't have a point, we need to find it
            (treePos.end - nameLength, treePos.end)
        Position(start, end, start)
      }
      source.atPos(position)
    }
  }
}

object SourceTree {
  def fromSymbol(sym: ClassSymbol, id: String = "")(implicit ctx: Context): List[SourceTree] = {
    if (sym == defn.SourceFileAnnot || // FIXME: No SourceFile annotation on SourceFile itself
        sym.sourceFile == null) // FIXME: We cannot deal with external projects yet
      Nil
    else {
      import ast.Trees._
      def sourceTreeOfClass(tree: tpd.Tree): Option[SourceNamedTree] = tree match {
        case PackageDef(_, stats) =>
          stats.flatMap(sourceTreeOfClass).headOption
        case tree: tpd.TypeDef if tree.symbol == sym =>
          val sourceFile = new SourceFile(sym.sourceFile, Codec.UTF8)
          Some(SourceNamedTree(tree, sourceFile))
        case _ =>
          None
      }

      def sourceImports(tree: tpd.Tree, sourceFile: SourceFile): List[SourceImportTree] = tree match {
        case PackageDef(_, stats) => stats.flatMap(sourceImports(_, sourceFile))
        case imp: tpd.Import => SourceImportTree(imp, sourceFile) :: Nil
        case _ => Nil
      }

      val tree = sym.rootTreeContaining(id)
      sourceTreeOfClass(tree) match {
        case Some(namedTree) => namedTree :: sourceImports(tree, namedTree.source)
        case None => Nil
      }
    }
  }
}
