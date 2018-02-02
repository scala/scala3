package dotty.tools
package dotc
package interactive

import scala.annotation.tailrec
import scala.collection._

import ast.{NavigateAST, Trees, tpd, untpd}
import core._, core.Decorators.{sourcePos => _, _}
import Contexts._, Flags._, Names._, NameOps._, Symbols._, SymDenotations._, Trees._, Types._
import util.Positions._, util.SourcePosition
import NameKinds.SimpleNameKind

/** High-level API to get information out of typed trees, designed to be used by IDEs.
 *
 *  @see `InteractiveDriver` to get typed trees from code.
 */
object Interactive {
  import ast.tpd._

  object Include { // should be an enum, really.
    type Set = Int
    val overridden = 1 // include trees whose symbol is overridden by `sym`
    val overriding = 2 // include trees whose symbol overrides `sym`
    val references = 4 // include references and not just definitions
  }

  /** Does this tree define a symbol ? */
  def isDefinition(tree: Tree) =
    tree.isInstanceOf[DefTree with NameTree]

  /** The type of the closest enclosing tree with a type containing position `pos`. */
  def enclosingType(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): Type = {
    val path = pathTo(trees, pos)
    if (path.isEmpty) NoType
    else path.head.tpe
  }
  /** The closest enclosing tree with a symbol containing position `pos`.
   */
  def enclosingTree(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): Tree =
    pathTo(trees, pos).dropWhile(!_.symbol.exists).headOption.getOrElse(tpd.EmptyTree)

  /** The source symbol of the closest enclosing tree with a symbol containing position `pos`.
   *
   *  @see sourceSymbol
   */
  def enclosingSourceSymbol(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): Symbol =
    sourceSymbol(enclosingTree(trees, pos).symbol)

  /** A symbol related to `sym` that is defined in source code.
   *
   *  @see enclosingSourceSymbol
   */
  @tailrec def sourceSymbol(sym: Symbol)(implicit ctx: Context): Symbol =
    if (!sym.exists)
      sym
    else if (sym.is(ModuleVal))
      sourceSymbol(sym.moduleClass) // The module val always has a zero-extent position
    else if (sym.is(Synthetic)) {
      val linked = sym.linkedClass
      if (linked.exists && !linked.is(Synthetic))
        linked
      else
        sourceSymbol(sym.owner)
    }
    else if (sym.isPrimaryConstructor)
      sourceSymbol(sym.owner)
    else sym

  /** Check if `tree` matches `sym`.
   *  This is the case if the symbol defined by `tree` equals `sym`,
   *  or the source symbol of tree equals sym,
   *  or `include` is `overridden`, and `tree` is overridden by `sym`,
   *  or `include` is `overriding`, and `tree` overrides `sym`.
   */
  def matchSymbol(tree: Tree, sym: Symbol, include: Include.Set)(implicit ctx: Context): Boolean = {

    def overrides(sym1: Symbol, sym2: Symbol) =
      sym1.owner.derivesFrom(sym2.owner) && sym1.overriddenSymbol(sym2.owner.asClass) == sym2

    (  sym == tree.symbol
    || sym.exists && sym == sourceSymbol(tree.symbol)
    || include != 0 && sym.name == tree.symbol.name && sym.owner != tree.symbol.owner
       && (  (include & Include.overridden) != 0 && overrides(sym, tree.symbol)
          || (include & Include.overriding) != 0 && overrides(tree.symbol, sym)
          )
    )
  }

  private def safely[T](op: => List[T]): List[T] =
    try op catch { case ex: TypeError => Nil }

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): (Int, List[Symbol]) = {
    val path = pathTo(trees, pos)
    val boundary = enclosingDefinitionInPath(path).symbol

    // FIXME: Get all declarations available in the current scope, not just
    // those from the enclosing class
    def scopeCompletions: List[Symbol] =
      boundary.enclosingClass match {
        case csym: ClassSymbol =>
          val classRef = csym.classInfo.appliedRef
          completions(classRef, boundary)
        case _ =>
          Nil
      }

    path.headOption.map {
      case sel @ Select(qual, name) =>
        // When completing "`a.foo`, return the members of `a`
        (sel.pos.point, completions(qual.tpe, boundary))
      case id @ Ident(name) =>
        (id.pos.point, scopeCompletions)
      case _ =>
        (0, scopeCompletions)
    }
    .getOrElse((0, Nil))
  }

  /** Possible completions of members of `prefix` which are accessible when called inside `boundary` */
  def completions(prefix: Type, boundary: Symbol)(implicit ctx: Context): List[Symbol] =
    safely {
      if (boundary != NoSymbol) {
        val boundaryCtx = ctx.withOwner(boundary)
        prefix.memberDenots(completionsFilter, (name, buf) =>
          buf ++= prefix.member(name).altsWith{ d =>
            !d.isAbsent &&
            !d.is(Synthetic) && !d.is(Artifact) &&
            d.symbol.isAccessibleFrom(prefix)(boundaryCtx)
          }
        ).map(_.symbol).toList
      }
      else Nil
    }

  /** Filter for names that should appear when looking for completions. */
  private[this] object completionsFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      !name.isConstructorName && name.toTermName.info.kind == SimpleNameKind
  }

  /** Find named trees with a non-empty position whose symbol match `sym` in `trees`.
   *
   *  Note that nothing will be found for symbols not defined in source code,
   *  use `sourceSymbol` to get a symbol related to `sym` that is defined in
   *  source code.
   */
  def namedTrees(trees: List[SourceTree], include: Include.Set, sym: Symbol)
   (implicit ctx: Context): List[SourceTree] =
    if (!sym.exists)
      Nil
    else
      namedTrees(trees, (include & Include.references) != 0, matchSymbol(_, sym, include))

  /** Find named trees with a non-empty position whose name contains `nameSubstring` in `trees`.
   *
   *  @param includeReferences  If true, include references and not just definitions
   */
  def namedTrees(trees: List[SourceTree], includeReferences: Boolean, nameSubstring: String)
   (implicit ctx: Context): List[SourceTree] =
    namedTrees(trees, includeReferences, _.show.toString.contains(nameSubstring))

  /** Find named trees with a non-empty position satisfying `treePredicate` in `trees`.
   *
   *  @param includeReferences  If true, include references and not just definitions
   */
  def namedTrees(trees: List[SourceTree], includeReferences: Boolean, treePredicate: NameTree => Boolean)
    (implicit ctx: Context): List[SourceTree] = safely {
    val buf = new mutable.ListBuffer[SourceTree]

    trees foreach { case SourceTree(topTree, source) =>
      (new untpd.TreeTraverser {
        override def traverse(tree: untpd.Tree)(implicit ctx: Context) = {
          tree match {
            case utree: untpd.NameTree if tree.hasType =>
              val tree = utree.asInstanceOf[tpd.NameTree]
              if (tree.symbol.exists
                   && !tree.symbol.is(Synthetic)
                   && tree.pos.exists
                   && !tree.pos.isZeroExtent
                   && (includeReferences || isDefinition(tree))
                   && treePredicate(tree))
                buf += SourceTree(tree, source)
              traverseChildren(tree)
            case tree: untpd.Inlined =>
              traverse(tree.call)
            case _ =>
              traverseChildren(tree)
          }
        }
      }).traverse(topTree)
    }

    buf.toList
  }

  /** The reverse path to the node that closest encloses position `pos`,
   *  or `Nil` if no such path exists. If a non-empty path is returned it starts with
   *  the tree closest enclosing `pos` and ends with an element of `trees`.
   */
  def pathTo(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): List[Tree] =
    trees.find(_.pos.contains(pos)) match {
      case Some(tree) =>
        // FIXME: We shouldn't need a cast. Change NavigateAST.pathTo to return a List of Tree?
        val path = NavigateAST.pathTo(pos.pos, tree.tree, skipZeroExtent = true).asInstanceOf[List[untpd.Tree]]

        path.dropWhile(!_.hasType).asInstanceOf[List[tpd.Tree]]
      case None =>
        Nil
    }

  /** The first tree in the path that is a definition. */
  def enclosingDefinitionInPath(path: List[Tree])(implicit ctx: Context): Tree =
    path.find(_.isInstanceOf[DefTree]).getOrElse(EmptyTree)
}
